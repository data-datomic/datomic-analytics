/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/*
 * GIO TLS tests
 *
 * Copyright 2011 Collabora, Ltd.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * Public License along with this library; if not, see
 * <http://www.gnu.org/licenses/>.
 *
 * In addition, when the library is used with OpenSSL, a special
 * exception applies. Refer to the LICENSE_EXCEPTION file for details.
 *
 * Author: Stef Walter <stefw@collabora.co.uk>
 */

#include "config.h"

#include "mock-interaction.h"

#include <gio/gio.h>

#include <sys/types.h>
#include <string.h>

#ifdef BACKEND_IS_GNUTLS
#include <gnutls/gnutls.h>
#else
#include "openssl-include.h"
#endif

static const gchar *
tls_test_file_path (const char *name)
{
  const gchar *const_path;
  gchar *path;

  path = g_test_build_filename (G_TEST_DIST, "files", name, NULL);
  if (!g_path_is_absolute (path))
    {
      gchar *cwd, *abs;

      cwd = g_get_current_dir ();
      abs = g_build_filename (cwd, path, NULL);
      g_free (cwd);
      g_free (path);
      path = abs;
    }

  const_path = g_intern_string (path);
  g_free (path);
  return const_path;
}

#define TEST_DATA "You win again, gravity!\n"
#define TEST_DATA_LENGTH 24

typedef enum {
  WRITE_THEN_CLOSE,
  WRITE_THEN_WAIT,
  HANDSHAKE_ONLY
} ServerConnectionReceivedStrategy;

typedef struct {
  GMainContext *context;
  GMainLoop *loop;
  GSocketService *service;
  GTlsDatabase *database;
  GIOStream *server_connection;
  GIOStream *client_connection;
  GSocketConnectable *identity;
  GSocketAddress *address;
  GTlsAuthenticationMode auth_mode;
  gboolean rehandshake;
  GTlsCertificateFlags accept_flags;
  GError *read_error;
  GError *server_error;
  gboolean ignore_client_close_error;
  ServerConnectionReceivedStrategy connection_received_strategy;
  gboolean server_running;
  gboolean server_ever_handshaked;
  GTlsCertificate *server_certificate;
  const gchar * const *server_protocols;
  gulong incoming_connection_delay;

  char buf[128];
  gssize nread, nwrote;
} TestConnection;

static void
setup_connection (TestConnection *test, gconstpointer data)
{
  test->context = g_main_context_default ();
  test->loop = g_main_loop_new (test->context, FALSE);
  test->auth_mode = G_TLS_AUTHENTICATION_NONE;
}

/* Waits about 10 seconds for @var to be NULL/FALSE */
#define WAIT_UNTIL_UNSET(var)                                \
  if (var)                                                   \
    {                                                        \
      int i;                                                 \
                                                             \
      for (i = 0; i < 13 && (var); i++)                      \
        {                                                    \
          g_usleep (1000 * (1 << i));                        \
          g_main_context_iteration (NULL, FALSE);            \
        }                                                    \
                                                             \
      g_assert_true (!(var));                                \
    }

static void
wait_until_server_finished (TestConnection *test)
{
    WAIT_UNTIL_UNSET (test->server_running);
}

static void
teardown_connection (TestConnection *test, gconstpointer data)
{
  if (test->service)
    {
      g_socket_service_stop (test->service);
      /* The outstanding accept_async will hold a ref on test->service,
       * which we want to wait for it to release if we're valgrinding.
       */
      g_object_add_weak_pointer (G_OBJECT (test->service), (gpointer *)&test->service);
      g_object_unref (test->service);
      WAIT_UNTIL_UNSET (test->service);
    }

  if (test->server_connection)
    {
      WAIT_UNTIL_UNSET (test->server_running);

      g_object_add_weak_pointer (G_OBJECT (test->server_connection),
                                 (gpointer *)&test->server_connection);
      g_object_unref (test->server_connection);
      WAIT_UNTIL_UNSET (test->server_connection);
    }

  if (test->client_connection)
    {
      g_object_add_weak_pointer (G_OBJECT (test->client_connection),
                                 (gpointer *)&test->client_connection);
      g_object_unref (test->client_connection);
      WAIT_UNTIL_UNSET (test->client_connection);
    }

  if (test->database)
    {
      g_object_add_weak_pointer (G_OBJECT (test->database),
                                 (gpointer *)&test->database);
      g_object_unref (test->database);
      WAIT_UNTIL_UNSET (test->database);
    }

  g_clear_object (&test->address);
  g_clear_object (&test->identity);
  g_clear_object (&test->server_certificate);

  g_main_loop_unref (test->loop);

  g_clear_error (&test->read_error);
  g_clear_error (&test->server_error);
}

static void
start_server (TestConnection *test)
{
  GInetAddress *inet;
  GSocketAddress *addr;
  GInetSocketAddress *iaddr;
  GError *error = NULL;

  inet = g_inet_address_new_from_string ("127.0.0.1");
  addr = g_inet_socket_address_new (inet, 0);
  g_object_unref (inet);

  g_socket_listener_add_address (G_SOCKET_LISTENER (test->service), addr,
                                 G_SOCKET_TYPE_STREAM, G_SOCKET_PROTOCOL_TCP,
                                 NULL, &test->address, &error);
  g_assert_no_error (error);

  g_object_unref (addr);

  /* The hostname in test->identity matches the server certificate. */
  iaddr = G_INET_SOCKET_ADDRESS (test->address);
  test->identity = g_network_address_new ("server.example.com",
                                          g_inet_socket_address_get_port (iaddr));

  test->server_running = TRUE;
}

static gboolean
on_accept_certificate (GTlsConnection       *conn,
                       GTlsCertificate      *cert,
                       GTlsCertificateFlags  errors,
                       gpointer              user_data)
{
  TestConnection *test = user_data;

  g_assert_true (G_IS_TLS_CERTIFICATE (cert));

  return errors == test->accept_flags;
}

static void on_output_write_finish (GObject        *object,
                                    GAsyncResult   *res,
                                    gpointer        user_data);

static void
on_rehandshake_finish (GObject        *object,
                       GAsyncResult   *res,
                       gpointer        user_data)
{
  TestConnection *test = user_data;
  GError *error = NULL;
  GOutputStream *stream;

  g_tls_connection_handshake_finish (G_TLS_CONNECTION (object), res, &error);
  g_assert_no_error (error);

  stream = g_io_stream_get_output_stream (test->server_connection);
  g_output_stream_write_async (stream, TEST_DATA + TEST_DATA_LENGTH / 2,
                               TEST_DATA_LENGTH / 2,
                               G_PRIORITY_DEFAULT, NULL,
                               on_output_write_finish, test);
}

static void
on_server_close_finish (GObject        *object,
                        GAsyncResult   *res,
                        gpointer        user_data)
{
  TestConnection *test = user_data;
  GError *error = NULL;

  g_io_stream_close_finish (G_IO_STREAM (object), res, &error);
  // FIXME: https://gitlab.gnome.org/GNOME/glib-networking/issues/105
  // g_assert_no_error (error);

  test->server_running = FALSE;
}

static void
close_server_connection (TestConnection *test)
{
  g_io_stream_close_async (test->server_connection, G_PRIORITY_DEFAULT, NULL,
                           on_server_close_finish, test);
}

static void
on_output_write_finish (GObject        *object,
                        GAsyncResult   *res,
                        gpointer        user_data)
{
  TestConnection *test = user_data;

  g_assert_no_error (test->server_error);
  g_output_stream_write_finish (G_OUTPUT_STREAM (object), res, &test->server_error);

  if (!test->server_error && test->rehandshake)
    {
      test->rehandshake = FALSE;
      g_tls_connection_handshake_async (G_TLS_CONNECTION (test->server_connection),
                                        G_PRIORITY_DEFAULT, NULL,
                                        on_rehandshake_finish, test);
      return;
    }

  if (test->connection_received_strategy == WRITE_THEN_CLOSE)
    close_server_connection (test);
}

static void
on_server_handshake_finish (GObject      *object,
                            GAsyncResult *res,
                            gpointer      user_data)
{
  TestConnection *test = user_data;
  g_tls_connection_handshake_finish (G_TLS_CONNECTION (object), res, &test->server_error);
  g_assert_no_error (test->server_error);
  test->server_ever_handshaked = TRUE;
}

static gboolean
on_incoming_connection (GSocketService     *service,
                        GSocketConnection  *connection,
                        GObject            *source_object,
                        gpointer            user_data)
{
  TestConnection *test = user_data;
  GOutputStream *stream;
  GTlsCertificate *cert;
  GError *error = NULL;

  if (test->incoming_connection_delay != 0)
    g_usleep (test->incoming_connection_delay);

  g_assert_null (test->server_connection);
  test->server_connection = g_tls_server_connection_new (G_IO_STREAM (connection),
                                                         test->server_certificate, &error);
  g_assert_no_error (error);

  if (!test->server_certificate)
    {
      cert = g_tls_certificate_new_from_file (tls_test_file_path ("server-and-key.pem"), &error);
      g_assert_no_error (error);
      g_tls_connection_set_certificate (G_TLS_CONNECTION (test->server_connection), cert);
      g_object_unref (cert);
    }

  g_object_set (test->server_connection, "authentication-mode", test->auth_mode, NULL);
  g_signal_connect (test->server_connection, "accept-certificate",
                    G_CALLBACK (on_accept_certificate), test);

  if (test->database)
    g_tls_connection_set_database (G_TLS_CONNECTION (test->server_connection), test->database);

  if (test->server_protocols)
    {
      g_tls_connection_set_advertised_protocols (G_TLS_CONNECTION (test->server_connection),
                                                 test->server_protocols);
    }

  stream = g_io_stream_get_output_stream (test->server_connection);

  if (test->connection_received_strategy == WRITE_THEN_CLOSE ||
      test->connection_received_strategy == WRITE_THEN_WAIT)
    {
      g_output_stream_write_async (stream, TEST_DATA,
                                   test->rehandshake ? TEST_DATA_LENGTH / 2 : TEST_DATA_LENGTH,
                                   G_PRIORITY_DEFAULT, NULL,
                                   on_output_write_finish, test);
    }
  else
    {
      g_tls_connection_handshake_async (G_TLS_CONNECTION (test->server_connection),
                                        G_PRIORITY_DEFAULT, NULL,
                                        on_server_handshake_finish, test);
    }

  return FALSE;
}

static void
start_async_server_service (TestConnection                   *test,
                            GTlsAuthenticationMode            auth_mode,
                            ServerConnectionReceivedStrategy  connection_received_strategy)
{
  test->service = g_socket_service_new ();
  start_server (test);

  test->auth_mode = auth_mode;
  g_signal_connect (test->service, "incoming", G_CALLBACK (on_incoming_connection), test);

  test->connection_received_strategy = connection_received_strategy;
}

static GIOStream *
start_async_server_and_connect_to_it (TestConnection         *test,
                                      GTlsAuthenticationMode  auth_mode)
{
  GSocketClient *client;
  GError *error = NULL;
  GSocketConnection *connection;

  start_async_server_service (test, auth_mode, WRITE_THEN_CLOSE);

  client = g_socket_client_new ();
  connection = g_socket_client_connect (client, G_SOCKET_CONNECTABLE (test->address),
                                        NULL, &error);
  g_assert_no_error (error);
  g_object_unref (client);

  return G_IO_STREAM (connection);
}

static void
run_echo_server (GThreadedSocketService *service,
                 GSocketConnection      *connection,
                 GObject                *source_object,
                 gpointer                user_data)
{
  TestConnection *test = user_data;
  GTlsConnection *tlsconn;
  GTlsCertificate *cert;
  GError *error = NULL;
  GInputStream *istream;
  GOutputStream *ostream;
  gssize nread, nwrote, total;
  gchar buf[128];

  if (test->server_certificate)
    {
      cert = g_object_ref (test->server_certificate);
    }
  else
    {
      cert = g_tls_certificate_new_from_file (tls_test_file_path ("server-and-key.pem"), &error);
      g_assert_no_error (error);
    }

  test->server_connection = g_tls_server_connection_new (G_IO_STREAM (connection),
                                                         cert, &error);
  g_assert_no_error (error);
  g_object_unref (cert);

  tlsconn = G_TLS_CONNECTION (test->server_connection);
  g_tls_connection_handshake (tlsconn, NULL, &error);
  g_assert_no_error (error);

  istream = g_io_stream_get_input_stream (test->server_connection);
  ostream = g_io_stream_get_output_stream (test->server_connection);

  while (TRUE)
    {
      nread = g_input_stream_read (istream, buf, sizeof (buf), NULL, &error);
      g_assert_no_error (error);
      g_assert_cmpint (nread, >=, 0);

      if (nread == 0)
        break;

      for (total = 0; total < nread; total += nwrote)
        {
          nwrote = g_output_stream_write (ostream, buf + total, nread - total, NULL, &error);
          g_assert_no_error (error);
        }

      if (test->rehandshake)
        {
          test->rehandshake = FALSE;
          g_tls_connection_handshake (tlsconn, NULL, &error);
          g_assert_no_error (error);
        }
    }

  g_io_stream_close (test->server_connection, NULL, &error);
  g_assert_no_error (error);
  test->server_running = FALSE;
}

static void
start_echo_server_service (TestConnection *test)
{
  test->service = g_threaded_socket_service_new (5);
  start_server (test);

  g_signal_connect (test->service, "run", G_CALLBACK (run_echo_server), test);
}

static GIOStream *
start_echo_server_and_connect_to_it (TestConnection *test)
{
  GSocketClient *client;
  GError *error = NULL;
  GSocketConnection *connection;

  start_echo_server_service (test);

  client = g_socket_client_new ();
  connection = g_socket_client_connect (client, G_SOCKET_CONNECTABLE (test->address),
                                        NULL, &error);
  g_assert_no_error (error);
  g_object_unref (client);

  return G_IO_STREAM (connection);
}

static void
on_client_connection_close_finish (GObject        *object,
                                   GAsyncResult   *res,
                                   gpointer        user_data)
{
  TestConnection *test = user_data;
  GError *error = NULL;

  g_io_stream_close_finish (G_IO_STREAM (object), res, &error);

  /* FIXME: When running test_client_auth_failure(), GnuTLS throws a
   * G_TLS_CERTIFICATE_REQUIRED error here for TLS 1.3, but no error for TLS
   * 1.2. What's up with this difference? Can we have consistent errors?
   */
  if (!test->ignore_client_close_error)
    g_assert_no_error (error);

  g_main_loop_quit (test->loop);
}

static void
on_input_read_finish (GObject        *object,
                      GAsyncResult   *res,
                      gpointer        user_data)
{
  TestConnection *test = user_data;
  gchar *line, *check;

  line = g_data_input_stream_read_line_finish (G_DATA_INPUT_STREAM (object), res,
                                               NULL, &test->read_error);
  if (!test->read_error)
    {
      g_assert_nonnull (line);

      check = g_strdup (TEST_DATA);
      g_strstrip (check);
      g_assert_cmpstr (line, ==, check);
      g_free (check);
      g_free (line);
    }

  g_io_stream_close_async (test->client_connection, G_PRIORITY_DEFAULT,
                           NULL, on_client_connection_close_finish, test);
}

static void
read_test_data_async (TestConnection *test)
{
  GDataInputStream *stream;

  stream = g_data_input_stream_new (g_io_stream_get_input_stream (test->client_connection));
  g_assert_nonnull (stream);

  g_data_input_stream_read_line_async (stream, G_PRIORITY_DEFAULT, NULL,
                                       on_input_read_finish, test);
  g_object_unref (stream);
}

static void
test_basic_connection (TestConnection *test,
                       gconstpointer   data)
{
  GIOStream *connection;
  GError *error = NULL;

  connection = start_async_server_and_connect_to_it (test, G_TLS_AUTHENTICATION_NONE);
  test->client_connection = g_tls_client_connection_new (connection, test->identity, &error);
  g_assert_no_error (error);
  g_object_unref (connection);

  /* No validation at all in this test */
  g_tls_client_connection_set_validation_flags (G_TLS_CLIENT_CONNECTION (test->client_connection),
                                                0);

  read_test_data_async (test);
  g_main_loop_run (test->loop);
  wait_until_server_finished (test);

  g_assert_no_error (test->read_error);
  g_assert_no_error (test->server_error);
}

static void
test_verified_connection (TestConnection *test,
                          gconstpointer   data)
{
  GIOStream *connection;
  GError *error = NULL;

  test->database = g_tls_file_database_new (tls_test_file_path ("ca-roots.pem"), &error);
  g_assert_no_error (error);
  g_assert_nonnull (test->database);

  connection = start_async_server_and_connect_to_it (test, G_TLS_AUTHENTICATION_NONE);
  test->client_connection = g_tls_client_connection_new (connection, test->identity, &error);
  g_assert_no_error (error);
  g_assert_nonnull (test->client_connection);
  g_object_unref (connection);

  g_tls_connection_set_database (G_TLS_CONNECTION (test->client_connection), test->database);

  /* All validation in this test */
  g_tls_client_connection_set_validation_flags (G_TLS_CLIENT_CONNECTION (test->client_connection),
                                                G_TLS_CERTIFICATE_VALIDATE_ALL);

  read_test_data_async (test);
  g_main_loop_run (test->loop);
  wait_until_server_finished (test);

  g_assert_no_error (test->read_error);
  g_assert_no_error (test->server_error);
}

static void
test_verified_chain (TestConnection *test,
                     gconstpointer   data)
{
  GTlsBackend *backend;
  GTlsCertificate *server_cert;
  GTlsCertificate *intermediate_cert;
  char *cert_data = NULL;
  char *key_data = NULL;
  GError *error = NULL;

  backend = g_tls_backend_get_default ();

  /* Prepare the intermediate cert. */
  intermediate_cert = g_tls_certificate_new_from_file (tls_test_file_path ("intermediate-ca.pem"), &error);
  g_assert_no_error (error);
  g_assert_nonnull (intermediate_cert);

  /* Prepare the server cert. */
  g_clear_pointer (&cert_data, g_free);
  g_file_get_contents (tls_test_file_path ("server-intermediate.pem"),
                       &cert_data, NULL, &error);
  g_assert_no_error (error);
  g_assert_nonnull (cert_data);

  g_file_get_contents (tls_test_file_path ("server-intermediate-key.pem"),
                       &key_data, NULL, &error);
  g_assert_no_error (error);
  g_assert_nonnull (key_data);

  server_cert = g_initable_new (g_tls_backend_get_certificate_type (backend),
                                NULL, &error,
                                "issuer", intermediate_cert,
                                "certificate-pem", cert_data,
                                "private-key-pem", key_data,
                                NULL);
  g_assert_no_error (error);
  g_assert_nonnull (server_cert);

  g_object_unref (intermediate_cert);
  g_free (cert_data);
  g_free (key_data);

  test->server_certificate = server_cert;
  test_verified_connection (test, data);
}

static void
test_verified_chain_with_redundant_root_cert (TestConnection *test,
                                              gconstpointer   data)
{
  GTlsBackend *backend;
  GTlsCertificate *server_cert;
  GTlsCertificate *intermediate_cert;
  GTlsCertificate *root_cert;
  char *cert_data = NULL;
  char *key_data = NULL;
  GError *error = NULL;

  backend = g_tls_backend_get_default ();

  /* The root is redundant. It should not hurt anything. */
  root_cert = g_tls_certificate_new_from_file (tls_test_file_path ("ca.pem"), &error);
  g_assert_no_error (error);
  g_assert_nonnull (root_cert);

  /* Prepare the intermediate cert. */
  g_file_get_contents (tls_test_file_path ("intermediate-ca.pem"),
                       &cert_data, NULL, &error);
  g_assert_no_error (error);
  g_assert_nonnull (cert_data);

  intermediate_cert = g_initable_new (g_tls_backend_get_certificate_type (backend),
                                      NULL, &error,
                                      "issuer", root_cert,
                                      "certificate-pem", cert_data,
                                      NULL);
  g_assert_no_error (error);
  g_assert_nonnull (intermediate_cert);

  /* Prepare the server cert. */
  g_clear_pointer (&cert_data, g_free);
  g_file_get_contents (tls_test_file_path ("server-intermediate.pem"),
                       &cert_data, NULL, &error);
  g_assert_no_error (error);
  g_assert_nonnull (cert_data);

  g_file_get_contents (tls_test_file_path ("server-intermediate-key.pem"),
                       &key_data, NULL, &error);
  g_assert_no_error (error);
  g_assert_nonnull (key_data);

  server_cert = g_initable_new (g_tls_backend_get_certificate_type (backend),
                                NULL, &error,
                                "issuer", intermediate_cert,
                                "certificate-pem", cert_data,
                                "private-key-pem", key_data,
                                NULL);
  g_assert_no_error (error);
  g_assert_nonnull (server_cert);

  g_object_unref (intermediate_cert);
  g_object_unref (root_cert);
  g_free (cert_data);
  g_free (key_data);

  test->server_certificate = server_cert;
  test_verified_connection (test, data);
}

static void
test_verified_chain_with_duplicate_server_cert (TestConnection *test,
                                                gconstpointer   data)
{
  /* This is another common server misconfiguration. Apache reads certificates
   * from two configuration files: one for the server cert, and one for the rest
   * of the chain. If the server cert is pasted into both files, it will be sent
   * twice. We should be tolerant of this. */

  GTlsBackend *backend;
  GTlsCertificate *server_cert;
  GTlsCertificate *extra_server_cert;
  GTlsCertificate *intermediate_cert;
  char *cert_data = NULL;
  char *key_data = NULL;
  GError *error = NULL;

  backend = g_tls_backend_get_default ();

  /* Prepare the intermediate cert. */
  intermediate_cert = g_tls_certificate_new_from_file (tls_test_file_path ("intermediate-ca.pem"), &error);
  g_assert_no_error (error);
  g_assert_nonnull (intermediate_cert);

  /* Prepare the server cert. */
  g_clear_pointer (&cert_data, g_free);
  g_file_get_contents (tls_test_file_path ("server-intermediate.pem"),
                       &cert_data, NULL, &error);
  g_assert_no_error (error);
  g_assert_nonnull (cert_data);

  g_file_get_contents (tls_test_file_path ("server-intermediate-key.pem"),
                       &key_data, NULL, &error);
  g_assert_no_error (error);
  g_assert_nonnull (key_data);

  server_cert = g_initable_new (g_tls_backend_get_certificate_type (backend),
                                NULL, &error,
                                "issuer", intermediate_cert,
                                "certificate-pem", cert_data,
                                NULL);
  g_assert_no_error (error);
  g_assert_nonnull (server_cert);

  /* Prepare the server cert... again. Private key must go on this one. */
  extra_server_cert = g_initable_new (g_tls_backend_get_certificate_type (backend),
                                      NULL, &error,
                                      "issuer", server_cert,
                                      "certificate-pem", cert_data,
                                      "private-key-pem", key_data,
                                      NULL);
  g_assert_no_error (error);
  g_assert_nonnull (extra_server_cert);

  g_object_unref (intermediate_cert);
  g_object_unref (server_cert);
  g_free (cert_data);
  g_free (key_data);

  test->server_certificate = extra_server_cert;
  test_verified_connection (test, data);
}

static void
test_verified_unordered_chain (TestConnection *test,
                               gconstpointer   data)
{
  GTlsBackend *backend;
  GTlsCertificate *server_cert;
  GTlsCertificate *intermediate_cert;
  GTlsCertificate *root_cert;
  char *cert_data = NULL;
  char *key_data = NULL;
  GError *error = NULL;

  backend = g_tls_backend_get_default ();

  /* Prepare the intermediate cert (to be sent last, out of order)! */
  intermediate_cert = g_tls_certificate_new_from_file (tls_test_file_path ("intermediate-ca.pem"),
                                                       &error);
  g_assert_no_error (error);
  g_assert_nonnull (intermediate_cert);

  g_file_get_contents (tls_test_file_path ("ca.pem"), &cert_data, NULL, &error);
  g_assert_no_error (error);
  g_assert_nonnull (cert_data);

  /* Prepare the root cert (to be sent in the middle of the chain). */
  root_cert = g_initable_new (g_tls_backend_get_certificate_type (backend),
                              NULL, &error,
                              "issuer", intermediate_cert,
                              "certificate-pem", cert_data,
                              NULL);
  g_assert_no_error (error);
  g_assert_nonnull (root_cert);

  g_clear_pointer (&cert_data, g_free);
  g_file_get_contents (tls_test_file_path ("server-intermediate.pem"),
                       &cert_data, NULL, &error);
  g_assert_no_error (error);
  g_assert_nonnull (cert_data);

  g_file_get_contents (tls_test_file_path ("server-intermediate-key.pem"),
                       &key_data, NULL, &error);
  g_assert_no_error (error);
  g_assert_nonnull (key_data);

  /* Prepare the server cert. */
  server_cert = g_initable_new (g_tls_backend_get_certificate_type (backend),
                                NULL, &error,
                                "issuer", root_cert,
                                "certificate-pem", cert_data,
                                "private-key-pem", key_data,
                                NULL);
  g_assert_no_error (error);
  g_assert_nonnull (server_cert);

  g_object_unref (intermediate_cert);
  g_object_unref (root_cert);
  g_free (cert_data);
  g_free (key_data);

  test->server_certificate = server_cert;
  test_verified_connection (test, data);
}

static void
test_verified_chain_with_alternative_ca_cert (TestConnection *test,
                                              gconstpointer   data)
{
  GTlsBackend *backend;
  GTlsCertificate *server_cert;
  GTlsCertificate *intermediate_cert;
  GTlsCertificate *root_cert;
  char *cert_data = NULL;
  char *key_data = NULL;
  GError *error = NULL;

  backend = g_tls_backend_get_default ();

  /* This "root" cert is issued by a CA that is not in the trust store. So it's
   * not really a root, but it has the same public key as a cert in the trust
   * store. If the client insists on a traditional chain of trust, this will
   * fail, since the issuer is untrusted. */
  root_cert = g_tls_certificate_new_from_file (tls_test_file_path ("ca-alternative.pem"), &error);
  g_assert_no_error (error);
  g_assert_nonnull (root_cert);

  /* Prepare the intermediate cert. Modern TLS libraries are expected to notice
   * that it is signed by the same public key as a certificate in the root
   * store, and accept the certificate, ignoring the untrusted "root" sent next
   * in the chain, which servers send for compatibility with clients that don't
   * have the new CA cert in the trust store yet. (In this scenario, the old
   * client still trusts the old CA cert.) */
  g_file_get_contents (tls_test_file_path ("intermediate-ca.pem"),
                       &cert_data, NULL, &error);
  g_assert_no_error (error);
  g_assert_nonnull (cert_data);

  intermediate_cert = g_initable_new (g_tls_backend_get_certificate_type (backend),
                                      NULL, &error,
                                      "issuer", root_cert,
                                      "certificate-pem", cert_data,
                                      NULL);
  g_assert_no_error (error);
  g_assert_nonnull (intermediate_cert);

  /* Prepare the server cert. */
  g_clear_pointer (&cert_data, g_free);
  g_file_get_contents (tls_test_file_path ("server-intermediate.pem"),
                       &cert_data, NULL, &error);
  g_assert_no_error (error);
  g_assert_nonnull (cert_data);

  g_file_get_contents (tls_test_file_path ("server-intermediate-key.pem"),
                       &key_data, NULL, &error);
  g_assert_no_error (error);
  g_assert_nonnull (key_data);

  server_cert = g_initable_new (g_tls_backend_get_certificate_type (backend),
                                NULL, &error,
                                "issuer", intermediate_cert,
                                "certificate-pem", cert_data,
                                "private-key-pem", key_data,
                                NULL);
  g_assert_no_error (error);
  g_assert_nonnull (server_cert);

  g_object_unref (intermediate_cert);
  g_object_unref (root_cert);
  g_free (cert_data);
  g_free (key_data);

  test->server_certificate = server_cert;
  test_verified_connection (test, data);
}

static void
test_invalid_chain_with_alternative_ca_cert (TestConnection *test,
                                             gconstpointer   data)
{
  GTlsBackend *backend;
  GTlsCertificate *server_cert;
  GTlsCertificate *root_cert;
  GIOStream *connection;
  char *cert_data = NULL;
  char *key_data = NULL;
  GError *error = NULL;

  backend = g_tls_backend_get_default ();

  /* This certificate has the same public key as a certificate in the root store. */
  root_cert = g_tls_certificate_new_from_file (tls_test_file_path ("ca-alternative.pem"), &error);
  g_assert_no_error (error);
  g_assert_nonnull (root_cert);

  /* The intermediate cert is not sent. The chain should be rejected, since without intermediate.pem
   * there is no proof that ca-alternative.pem signed server-intermediate.pem. */
  g_file_get_contents (tls_test_file_path ("server-intermediate.pem"),
                       &cert_data, NULL, &error);
  g_assert_no_error (error);
  g_assert_nonnull (cert_data);

  g_file_get_contents (tls_test_file_path ("server-intermediate-key.pem"),
                       &key_data, NULL, &error);
  g_assert_no_error (error);
  g_assert_nonnull (key_data);

  server_cert = g_initable_new (g_tls_backend_get_certificate_type (backend),
                                NULL, &error,
                                "issuer", root_cert,
                                "certificate-pem", cert_data,
                                "private-key-pem", key_data,
                                NULL);
  g_assert_no_error (error);
  g_assert_nonnull (server_cert);

  g_object_unref (root_cert);
  g_free (cert_data);
  g_free (key_data);

  test->server_certificate = server_cert;
  connection = start_async_server_and_connect_to_it (test, G_TLS_AUTHENTICATION_NONE);
  test->client_connection = g_tls_client_connection_new (connection, test->identity, &error);
  g_assert_no_error (error);
  g_assert_nonnull (test->client_connection);
  g_object_unref (connection);

  g_tls_connection_set_database (G_TLS_CONNECTION (test->client_connection), test->database);

  /* Make sure this test doesn't expire. */
  g_tls_client_connection_set_validation_flags (G_TLS_CLIENT_CONNECTION (test->client_connection),
                                                G_TLS_CERTIFICATE_VALIDATE_ALL & ~G_TLS_CERTIFICATE_EXPIRED);

  read_test_data_async (test);
  g_main_loop_run (test->loop);
  wait_until_server_finished (test);

  g_assert_error (test->read_error, G_TLS_ERROR, G_TLS_ERROR_BAD_CERTIFICATE);

#ifdef BACKEND_IS_GNUTLS
  g_assert_error (test->server_error, G_TLS_ERROR, G_TLS_ERROR_NOT_TLS);
#elif defined(BACKEND_IS_OPENSSL)
  /* FIXME: This is not OK. There should be a NOT_TLS errors. But some times
   * we either get no error or BROKEN_PIPE
   */
#endif
}

static void
on_notify_accepted_cas (GObject *obj,
                        GParamSpec *spec,
                        gpointer user_data)
{
  gboolean *changed = user_data;
  *changed = TRUE;
}

static void
test_client_auth_connection (TestConnection *test,
                             gconstpointer   data)
{
  GIOStream *connection;
  GError *error = NULL;
  GTlsCertificate *cert;
  GTlsCertificate *peer;
  gboolean cas_changed;
  GSocketClient *client;

  test->database = g_tls_file_database_new (tls_test_file_path ("ca-roots.pem"), &error);
  g_assert_no_error (error);
  g_assert_nonnull (test->database);

  connection = start_async_server_and_connect_to_it (test, G_TLS_AUTHENTICATION_REQUIRED);
  test->client_connection = g_tls_client_connection_new (connection, test->identity, &error);
  g_assert_no_error (error);
  g_assert_nonnull (test->client_connection);
  g_object_unref (connection);

  g_tls_connection_set_database (G_TLS_CONNECTION (test->client_connection), test->database);

  cert = g_tls_certificate_new_from_file (tls_test_file_path ("client-and-key.pem"), &error);
  g_assert_no_error (error);

  g_tls_connection_set_certificate (G_TLS_CONNECTION (test->client_connection), cert);

  /* All validation in this test */
  g_tls_client_connection_set_validation_flags (G_TLS_CLIENT_CONNECTION (test->client_connection),
                                                G_TLS_CERTIFICATE_VALIDATE_ALL);

  cas_changed = FALSE;
  g_signal_connect (test->client_connection, "notify::accepted-cas",
                    G_CALLBACK (on_notify_accepted_cas), &cas_changed);

  read_test_data_async (test);
  g_main_loop_run (test->loop);
  wait_until_server_finished (test);

  g_assert_no_error (test->read_error);
  g_assert_no_error (test->server_error);

  peer = g_tls_connection_get_peer_certificate (G_TLS_CONNECTION (test->server_connection));
  g_assert_nonnull (peer);
  g_assert_true (g_tls_certificate_is_same (peer, cert));
  g_assert_true (cas_changed);

  g_object_unref (cert);
  g_object_unref (test->client_connection);
  g_clear_object (&test->server_connection);

  /* Now start a new connection to the same server with a different client cert */
  client = g_socket_client_new ();
  connection = G_IO_STREAM (g_socket_client_connect (client, G_SOCKET_CONNECTABLE (test->address),
                                                     NULL, &error));
  g_assert_no_error (error);
  g_object_unref (client);
  test->client_connection = g_tls_client_connection_new (connection, test->identity, &error);
  g_assert_no_error (error);
  g_assert_nonnull (test->client_connection);
  g_object_unref (connection);

  g_tls_client_connection_set_validation_flags (G_TLS_CLIENT_CONNECTION (test->client_connection),
                                                0);
  cert = g_tls_certificate_new_from_file (tls_test_file_path ("client2-and-key.pem"), &error);
  g_assert_no_error (error);
  g_tls_connection_set_certificate (G_TLS_CONNECTION (test->client_connection), cert);
  g_object_unref (cert);
  g_tls_connection_set_database (G_TLS_CONNECTION (test->client_connection), test->database);

  read_test_data_async (test);
  g_main_loop_run (test->loop);
  wait_until_server_finished (test);

  g_assert_no_error (test->read_error);
  g_assert_no_error (test->server_error);

  /* peer should see the second client cert */
  peer = g_tls_connection_get_peer_certificate (G_TLS_CONNECTION (test->server_connection));
  g_assert_nonnull (peer);
  g_assert_true (g_tls_certificate_is_same (peer, cert));
}

static void
test_client_auth_rehandshake (TestConnection *test,
                              gconstpointer   data)
{
#ifdef BACKEND_IS_OPENSSL
  /* FIXME: this doesn't make sense, we should support safe renegotation */
  g_test_skip ("the server avoids rehandshake to avoid the security problem CVE-2009-3555");
  return;
#endif

  test->rehandshake = TRUE;
  test_client_auth_connection (test, data);
}

static void
test_client_auth_failure (TestConnection *test,
                          gconstpointer   data)
{
  GIOStream *connection;
  GError *error = NULL;
  gboolean accepted_changed;
  GSocketClient *client;
  GTlsCertificate *cert;
  GTlsCertificate *peer;
  GTlsInteraction *interaction;

  test->database = g_tls_file_database_new (tls_test_file_path ("ca-roots.pem"), &error);
  g_assert_no_error (error);
  g_assert_nonnull (test->database);

  connection = start_async_server_and_connect_to_it (test, G_TLS_AUTHENTICATION_REQUIRED);
  test->client_connection = g_tls_client_connection_new (connection, test->identity, &error);
  g_assert_no_error (error);
  g_assert_nonnull (test->client_connection);
  g_object_unref (connection);

  g_tls_connection_set_database (G_TLS_CONNECTION (test->client_connection), test->database);

  /* No Certificate set */

  /* All validation in this test */
  g_tls_client_connection_set_validation_flags (G_TLS_CLIENT_CONNECTION (test->client_connection),
                                                G_TLS_CERTIFICATE_VALIDATE_ALL);

  accepted_changed = FALSE;
  g_signal_connect (test->client_connection, "notify::accepted-cas",
                    G_CALLBACK (on_notify_accepted_cas), &accepted_changed);

  test->ignore_client_close_error = TRUE;

  read_test_data_async (test);
  g_main_loop_run (test->loop);
  wait_until_server_finished (test);

  /* FIXME: We should always receive G_TLS_ERROR_CERTIFICATE_REQUIRED here. But
   * on our TLS 1.2 CI, sometimes we receive GNUTLS_E_PREMATURE_TERMINATION,
   * which we translate to G_TLS_ERROR_NOT_TLS because we have never handshaked
   * successfully. If the timing is different and it occurs after the handshake,
   * then we get G_TLS_ERROR_EOF. Sadly, I can't reproduce the issue locally, so
   * my odds of fixing it are slim to none. The connection is at least failing
   * as we expect, just not with the desired error.
   */
  if (!g_error_matches (test->read_error, G_TLS_ERROR, G_TLS_ERROR_NOT_TLS) &&
      !g_error_matches (test->read_error, G_TLS_ERROR, G_TLS_ERROR_EOF))
    {
      g_assert_error (test->read_error, G_TLS_ERROR, G_TLS_ERROR_CERTIFICATE_REQUIRED);
    }
  g_assert_error (test->server_error, G_TLS_ERROR, G_TLS_ERROR_CERTIFICATE_REQUIRED);

  g_assert_true (accepted_changed);

  g_object_unref (test->client_connection);
  g_clear_object (&test->server_connection);
  g_clear_error (&test->read_error);
  g_clear_error (&test->server_error);

  test->ignore_client_close_error = FALSE;

  /* Now start a new connection to the same server with a valid client cert;
   * this should succeed, and not use the cached failed session from above */
  client = g_socket_client_new ();
  connection = G_IO_STREAM (g_socket_client_connect (client, G_SOCKET_CONNECTABLE (test->address),
                                                     NULL, &error));
  g_assert_no_error (error);
  g_object_unref (client);
  test->client_connection = g_tls_client_connection_new (connection, test->identity, &error);
  g_assert_no_error (error);
  g_assert_nonnull (test->client_connection);
  g_object_unref (connection);

  g_tls_connection_set_database (G_TLS_CONNECTION (test->client_connection), test->database);

  /* Have the interaction return a certificate */
  cert = g_tls_certificate_new_from_file (tls_test_file_path ("client-and-key.pem"), &error);
  g_assert_no_error (error);
  interaction = mock_interaction_new_static_certificate (cert);
  g_tls_connection_set_interaction (G_TLS_CONNECTION (test->client_connection), interaction);
  g_object_unref (interaction);

  /* All validation in this test */
  g_tls_client_connection_set_validation_flags (G_TLS_CLIENT_CONNECTION (test->client_connection),
                                                G_TLS_CERTIFICATE_VALIDATE_ALL);

  accepted_changed = FALSE;
  g_signal_connect (test->client_connection, "notify::accepted-cas",
                    G_CALLBACK (on_notify_accepted_cas), &accepted_changed);

  read_test_data_async (test);
  g_main_loop_run (test->loop);
  wait_until_server_finished (test);

  g_assert_no_error (test->read_error);
  g_assert_no_error (test->server_error);

  peer = g_tls_connection_get_peer_certificate (G_TLS_CONNECTION (test->server_connection));
  g_assert_nonnull (peer);
  g_assert_true (g_tls_certificate_is_same (peer, cert));
  g_assert_true (accepted_changed);

  g_object_unref (cert);
}

static void
test_client_auth_fail_missing_client_private_key (TestConnection *test,
                                                  gconstpointer   data)
{
  GTlsCertificate *cert;
  GIOStream *connection;
  GError *error = NULL;

#ifdef BACKEND_IS_OPENSSL
  g_test_skip ("this new test does not work with openssl, more research needed");
  return;
#endif

  g_test_bug ("793712");

  test->database = g_tls_file_database_new (tls_test_file_path ("ca-roots.pem"), &error);
  g_assert_no_error (error);
  g_assert_nonnull (test->database);

  connection = start_async_server_and_connect_to_it (test, G_TLS_AUTHENTICATION_REQUIRED);
  test->client_connection = g_tls_client_connection_new (connection, test->identity, &error);
  g_assert_no_error (error);
  g_assert_nonnull (test->client_connection);
  g_object_unref (connection);

  g_tls_connection_set_database (G_TLS_CONNECTION (test->client_connection), test->database);

  /* Oops: we "accidentally" set client.pem rather than client-and-key.pem. The
   * connection will fail, but we should not crash.
   */
  cert = g_tls_certificate_new_from_file (tls_test_file_path ("client.pem"), &error);
  g_assert_no_error (error);

  g_tls_connection_set_certificate (G_TLS_CONNECTION (test->client_connection), cert);

  /* All validation in this test */
  g_tls_client_connection_set_validation_flags (G_TLS_CLIENT_CONNECTION (test->client_connection),
                                                G_TLS_CERTIFICATE_VALIDATE_ALL);

  read_test_data_async (test);
  g_main_loop_run (test->loop);
  wait_until_server_finished (test);

  g_assert_error (test->read_error, G_TLS_ERROR, G_TLS_ERROR_CERTIFICATE_REQUIRED);
  g_assert_error (test->server_error, G_TLS_ERROR, G_TLS_ERROR_NOT_TLS);
}

static void
test_client_auth_request_cert (TestConnection *test,
                               gconstpointer   data)
{
  GIOStream *connection;
  GError *error = NULL;
  GTlsCertificate *cert;
  GTlsCertificate *peer;
  GTlsInteraction *interaction;
  gboolean cas_changed;

  test->database = g_tls_file_database_new (tls_test_file_path ("ca-roots.pem"), &error);
  g_assert_no_error (error);
  g_assert_nonnull (test->database);

  connection = start_async_server_and_connect_to_it (test, G_TLS_AUTHENTICATION_REQUIRED);
  test->client_connection = g_tls_client_connection_new (connection, test->identity, &error);
  g_assert_no_error (error);
  g_assert_nonnull (test->client_connection);
  g_object_unref (connection);

  g_tls_connection_set_database (G_TLS_CONNECTION (test->client_connection), test->database);

  /* Have the interaction return a certificate */
  cert = g_tls_certificate_new_from_file (tls_test_file_path ("client-and-key.pem"), &error);
  g_assert_no_error (error);
  interaction = mock_interaction_new_static_certificate (cert);
  g_tls_connection_set_interaction (G_TLS_CONNECTION (test->client_connection), interaction);
  g_object_unref (interaction);

  /* All validation in this test */
  g_tls_client_connection_set_validation_flags (G_TLS_CLIENT_CONNECTION (test->client_connection),
                                                G_TLS_CERTIFICATE_VALIDATE_ALL);

  cas_changed = FALSE;
  g_signal_connect (test->client_connection, "notify::accepted-cas",
                    G_CALLBACK (on_notify_accepted_cas), &cas_changed);

  read_test_data_async (test);
  g_main_loop_run (test->loop);
  wait_until_server_finished (test);

  g_assert_no_error (test->read_error);
  g_assert_no_error (test->server_error);

  peer = g_tls_connection_get_peer_certificate (G_TLS_CONNECTION (test->server_connection));
  g_assert_nonnull (peer);
  g_assert_true (g_tls_certificate_is_same (peer, cert));
  g_assert_true (cas_changed);

  g_object_unref (cert);
}

static void
test_client_auth_request_fail (TestConnection *test,
                               gconstpointer   data)
{
  GIOStream *connection;
  GError *error = NULL;
  GTlsInteraction *interaction;

#ifdef BACKEND_IS_OPENSSL
  g_test_skip ("this new test does not work with openssl, more research needed");
  return;
#endif

  test->database = g_tls_file_database_new (tls_test_file_path ("ca-roots.pem"), &error);
  g_assert_no_error (error);
  g_assert_nonnull (test->database);

  connection = start_async_server_and_connect_to_it (test, G_TLS_AUTHENTICATION_REQUIRED);
  test->client_connection = g_tls_client_connection_new (connection, test->identity, &error);
  g_assert_no_error (error);
  g_assert_nonnull (test->client_connection);
  g_object_unref (connection);

  g_tls_connection_set_database (G_TLS_CONNECTION (test->client_connection), test->database);

  /* Have the interaction return an error */
  interaction = mock_interaction_new_static_error (G_FILE_ERROR, G_FILE_ERROR_ACCES, "Request message");
  g_tls_connection_set_interaction (G_TLS_CONNECTION (test->client_connection), interaction);
  g_object_unref (interaction);

  /* All validation in this test */
  g_tls_client_connection_set_validation_flags (G_TLS_CLIENT_CONNECTION (test->client_connection),
                                                G_TLS_CERTIFICATE_VALIDATE_ALL);

  test->ignore_client_close_error = TRUE;

  read_test_data_async (test);
  g_main_loop_run (test->loop);
  wait_until_server_finished (test);

  /* FIXME: We should always receive G_TLS_ERROR_CERTIFICATE_REQUIRED here. But
   * on our TLS 1.2 CI, sometimes we receive GNUTLS_E_PREMATURE_TERMINATION,
   * which we translate to G_TLS_ERROR_NOT_TLS because we have never handshaked
   * successfully. If the timing is different and it occurs after the handshake,
   * then we get G_TLS_ERROR_EOF. Sadly, I can't reproduce the issue locally, so
   * my odds of fixing it are slim to none. The connection is at least failing
   * as we expect, just not with the desired error.
   */
  if (!g_error_matches (test->read_error, G_TLS_ERROR, G_TLS_ERROR_NOT_TLS) &&
      !g_error_matches (test->read_error, G_TLS_ERROR, G_TLS_ERROR_EOF))
    {
      /* G_FILE_ERROR_ACCES is the error returned by our mock interaction object
       * when the GTlsInteraction's certificate request fails.
       */
      g_assert_error (test->read_error, G_FILE_ERROR, G_FILE_ERROR_ACCES);
    }
  g_assert_error (test->server_error, G_TLS_ERROR, G_TLS_ERROR_CERTIFICATE_REQUIRED);

  g_io_stream_close (test->server_connection, NULL, NULL);
  g_io_stream_close (test->client_connection, NULL, NULL);
}

static void
test_client_auth_request_none (TestConnection *test,
                               gconstpointer   data)
{
  GIOStream *connection;
  GError *error = NULL;

  test->database = g_tls_file_database_new (tls_test_file_path ("ca-roots.pem"), &error);
  g_assert_no_error (error);
  g_assert_nonnull (test->database);

  /* Request, but don't provide, a client certificate */
  connection = start_async_server_and_connect_to_it (test, G_TLS_AUTHENTICATION_REQUESTED);
  test->client_connection = g_tls_client_connection_new (connection, test->identity, &error);
  g_assert_no_error (error);
  g_assert_nonnull (test->client_connection);
  g_object_unref (connection);

  g_tls_connection_set_database (G_TLS_CONNECTION (test->client_connection), test->database);

  /* All validation in this test */
  g_tls_client_connection_set_validation_flags (G_TLS_CLIENT_CONNECTION (test->client_connection),
                                                G_TLS_CERTIFICATE_VALIDATE_ALL);

  read_test_data_async (test);
  g_main_loop_run (test->loop);
  wait_until_server_finished (test);

  /* The connection should succeed and everything should work. We only REQUESTED
   * authentication, in contrast to G_TLS_AUTHENTICATION_REQUIRED where this
   * should fail.
   */
  g_assert_no_error (test->read_error);
  g_assert_no_error (test->server_error);
}


static void
test_connection_no_database (TestConnection *test,
                             gconstpointer   data)
{
  GIOStream *connection;
  GError *error = NULL;

  connection = start_async_server_and_connect_to_it (test, G_TLS_AUTHENTICATION_NONE);
  test->client_connection = g_tls_client_connection_new (connection, test->identity, &error);
  g_assert_no_error (error);
  g_assert_nonnull (test->client_connection);
  g_object_unref (connection);

  /* Overrides loading of the default database */
  g_tls_connection_set_database (G_TLS_CONNECTION (test->client_connection), NULL);

  /* All validation in this test */
  g_tls_client_connection_set_validation_flags (G_TLS_CLIENT_CONNECTION (test->client_connection),
                                                G_TLS_CERTIFICATE_VALIDATE_ALL);

  test->accept_flags = G_TLS_CERTIFICATE_UNKNOWN_CA;
  g_signal_connect (test->client_connection, "accept-certificate",
                    G_CALLBACK (on_accept_certificate), test);

  read_test_data_async (test);
  g_main_loop_run (test->loop);
  wait_until_server_finished (test);

  g_assert_no_error (test->read_error);
  g_assert_no_error (test->server_error);
}

static void
handshake_failed_cb (GObject      *source,
                     GAsyncResult *result,
                     gpointer      user_data)
{
  TestConnection *test = user_data;
  GError *error = NULL;

  g_tls_connection_handshake_finish (G_TLS_CONNECTION (test->client_connection),
                                     result, &error);
  g_assert_error (error, G_TLS_ERROR, G_TLS_ERROR_BAD_CERTIFICATE);
  g_clear_error (&error);

  g_main_loop_quit (test->loop);
}

static void
test_failed_connection (TestConnection *test,
                        gconstpointer   data)
{
  GIOStream *connection;
  GError *error = NULL;
  GSocketConnectable *bad_addr;

  connection = start_async_server_and_connect_to_it (test, G_TLS_AUTHENTICATION_NONE);

  bad_addr = g_network_address_new ("wrong.example.com", 80);
  test->client_connection = g_tls_client_connection_new (connection, bad_addr, &error);
  g_object_unref (bad_addr);
  g_assert_no_error (error);
  g_object_unref (connection);

  g_tls_connection_handshake_async (G_TLS_CONNECTION (test->client_connection),
                                    G_PRIORITY_DEFAULT, NULL,
                                    handshake_failed_cb, test);
  g_main_loop_run (test->loop);

  g_tls_client_connection_set_validation_flags (G_TLS_CLIENT_CONNECTION (test->client_connection),
                                                G_TLS_CERTIFICATE_VALIDATE_ALL);

  read_test_data_async (test);
  g_main_loop_run (test->loop);
  wait_until_server_finished (test);

  g_assert_error (test->read_error, G_TLS_ERROR, G_TLS_ERROR_BAD_CERTIFICATE);

#ifdef BACKEND_IS_GNUTLS
  g_assert_error (test->server_error, G_TLS_ERROR, G_TLS_ERROR_NOT_TLS);
#elif defined(BACKEND_IS_OPENSSL)
  /* FIXME: This is not OK. There should be a NOT_TLS errors. But some times
   * we either get no error or BROKEN_PIPE
   */
#endif
}

static void
socket_client_connected (GObject      *source,
                         GAsyncResult *result,
                         gpointer      user_data)
{
  TestConnection *test = user_data;
  GSocketConnection *connection;
  GError *error = NULL;

  connection = g_socket_client_connect_finish (G_SOCKET_CLIENT (source),
                                               result, &error);
  g_assert_no_error (error);
  test->client_connection = G_IO_STREAM (connection);

  g_main_loop_quit (test->loop);
}

static void
test_connection_socket_client (TestConnection *test,
                               gconstpointer   data)
{
  GSocketClient *client;
  GTlsCertificateFlags flags;
  GSocketConnection *connection;
  GIOStream *base;
  GError *error = NULL;

  start_async_server_service (test, G_TLS_AUTHENTICATION_NONE, WRITE_THEN_CLOSE);
  client = g_socket_client_new ();
  g_socket_client_set_tls (client, TRUE);
  flags = G_TLS_CERTIFICATE_VALIDATE_ALL & ~G_TLS_CERTIFICATE_UNKNOWN_CA;
  /* test->address doesn't match the server's cert */
  flags = flags & ~G_TLS_CERTIFICATE_BAD_IDENTITY;
  g_socket_client_set_tls_validation_flags (client, flags);

  g_socket_client_connect_async (client, G_SOCKET_CONNECTABLE (test->address),
                                 NULL, socket_client_connected, test);
  g_main_loop_run (test->loop);
  wait_until_server_finished (test);

  connection = (GSocketConnection *)test->client_connection;
  test->client_connection = NULL;

  g_assert_true (G_IS_TCP_WRAPPER_CONNECTION (connection));
  base = g_tcp_wrapper_connection_get_base_io_stream (G_TCP_WRAPPER_CONNECTION (connection));
  g_assert_true (G_IS_TLS_CONNECTION (base));

  g_io_stream_close (G_IO_STREAM (connection), NULL, &error);
  g_assert_no_error (error);
  g_object_unref (connection);

  g_object_unref (client);
}

static void
socket_client_failed (GObject      *source,
                      GAsyncResult *result,
                      gpointer      user_data)
{
  TestConnection *test = user_data;
  GError *error = NULL;

  g_socket_client_connect_finish (G_SOCKET_CLIENT (source),
                                  result, &error);
  g_assert_error (error, G_TLS_ERROR, G_TLS_ERROR_BAD_CERTIFICATE);
  g_clear_error (&error);

  g_main_loop_quit (test->loop);
}

static void
test_connection_socket_client_failed (TestConnection *test,
                                      gconstpointer   data)
{
  GSocketClient *client;

  start_async_server_service (test, G_TLS_AUTHENTICATION_NONE, WRITE_THEN_CLOSE);
  client = g_socket_client_new ();
  g_socket_client_set_tls (client, TRUE);
  /* this time we don't adjust the validation flags */

  g_socket_client_connect_async (client, G_SOCKET_CONNECTABLE (test->address),
                                 NULL, socket_client_failed, test);
  g_main_loop_run (test->loop);
  wait_until_server_finished (test);

#ifdef BACKEND_IS_GNUTLS
  g_assert_error (test->server_error, G_TLS_ERROR, G_TLS_ERROR_NOT_TLS);
#else
  /* FIXME: This is not OK. There should be a NOT_TLS errors. But some times
   * we either get no error or BROKEN_PIPE
   */
#endif

  g_object_unref (client);
}

static gboolean
socket_client_timed_out_write (gpointer user_data)
{
  TestConnection *test = user_data;
  GInputStream *input_stream;
  GOutputStream *output_stream;
  GError *error = NULL;
  gchar buffer[TEST_DATA_LENGTH];
  gssize size;

  input_stream = g_io_stream_get_input_stream (test->client_connection);
  output_stream = g_io_stream_get_output_stream (test->client_connection);

  /* read TEST_DATA_LENGTH once */
  size = g_input_stream_read (input_stream, &buffer, TEST_DATA_LENGTH,
                              NULL, &error);
  if (error)
    {
      /* This should very rarely ever happen, but in practice it can take more
       * than one second to read under heavy load, or when running many tests
       * simultaneously, so don't fail if this happens.
       */
      g_assert_error (error, G_IO_ERROR, G_IO_ERROR_TIMED_OUT);
      g_assert_cmpint (size, ==, -1);
      g_clear_error (&error);
    }
  else
    {
      g_assert_no_error (error);
      g_assert_cmpint (size, ==, TEST_DATA_LENGTH);

      /* read TEST_DATA_LENGTH again to cause the time out */
      size = g_input_stream_read (input_stream, &buffer, TEST_DATA_LENGTH,
                                  NULL, &error);
      g_assert_error (error, G_IO_ERROR, G_IO_ERROR_TIMED_OUT);
      g_assert_cmpint (size, ==, -1);
      g_clear_error (&error);
    }

  /* write after a timeout, session should still be valid */
  size = g_output_stream_write (output_stream, TEST_DATA, TEST_DATA_LENGTH,
                                NULL, &error);
  g_assert_no_error (error);
  g_assert_cmpint (size, ==, TEST_DATA_LENGTH);

  g_main_loop_quit (test->loop);

  return G_SOURCE_REMOVE;
}

static void
socket_client_timed_out_write_connected (GObject      *source,
                                         GAsyncResult *result,
                                         gpointer      user_data)
{
  TestConnection *test = user_data;
  GSocketConnection *connection;
  GError *error = NULL;

  connection = g_socket_client_connect_finish (G_SOCKET_CLIENT (source),
                                               result, &error);
  g_assert_no_error (error);
  test->client_connection = G_IO_STREAM (connection);

  /* We need to use an idle callback here to guarantee that the upcoming call
   * to g_input_stream_read() executes on the next iteration of the main
   * context. Otherwise, we could deadlock ourselves: the read would not be able
   * to complete if GTask executes socket_client_timed_out_write_connected()
   * using g_task_return_now() instead of posting the invocation to the next
   * iteration of the main context, because the server will not progress until
   * the main context is iterated, but iteration would be blocked waiting for
   * client's read to complete.
   */
  g_idle_add (socket_client_timed_out_write, test);
}

static void
test_connection_read_time_out_write (TestConnection *test,
                                     gconstpointer   data)
{
  GSocketClient *client;
  GTlsCertificateFlags flags;
  GSocketConnection *connection;
  GIOStream *base;
  GError *error = NULL;

  /* Don't close the server connection after writing TEST_DATA. */
  start_async_server_service (test, G_TLS_AUTHENTICATION_NONE, WRITE_THEN_WAIT);
  client = g_socket_client_new ();
  /* Set a 1 second time out on the socket */
  g_socket_client_set_timeout (client, 1);
  g_socket_client_set_tls (client, TRUE);
  flags = G_TLS_CERTIFICATE_VALIDATE_ALL & ~G_TLS_CERTIFICATE_UNKNOWN_CA;
  /* test->address doesn't match the server's cert */
  flags = flags & ~G_TLS_CERTIFICATE_BAD_IDENTITY;
  g_socket_client_set_tls_validation_flags (client, flags);

  g_socket_client_connect_async (client, G_SOCKET_CONNECTABLE (test->address),
                                 NULL, socket_client_timed_out_write_connected, test);

  g_main_loop_run (test->loop);

  /* Close the server now */
  close_server_connection (test);

  connection = (GSocketConnection *)test->client_connection;
  test->client_connection = NULL;

  g_assert_true (G_IS_TCP_WRAPPER_CONNECTION (connection));
  base = g_tcp_wrapper_connection_get_base_io_stream (G_TCP_WRAPPER_CONNECTION (connection));
  g_assert_true (G_IS_TLS_CONNECTION (base));

  g_io_stream_close (G_IO_STREAM (connection), NULL, &error);
  g_assert_no_error (error);
  g_object_unref (connection);

  g_object_unref (client);
}

static void
simul_async_read_complete (GObject      *object,
                           GAsyncResult *result,
                           gpointer      user_data)
{
  TestConnection *test = user_data;
  gssize nread;
  GError *error = NULL;

  nread = g_input_stream_read_finish (G_INPUT_STREAM (object),
                                      result, &error);
  g_assert_no_error (error);

  test->nread += nread;
  g_assert_cmpint (test->nread, <=, TEST_DATA_LENGTH);

  if (test->nread == TEST_DATA_LENGTH)
    {
      g_io_stream_close (test->client_connection, NULL, &error);
      g_assert_no_error (error);
      g_main_loop_quit (test->loop);
    }
  else
    {
      g_input_stream_read_async (G_INPUT_STREAM (object),
                                 test->buf + test->nread,
                                 TEST_DATA_LENGTH / 2,
                                 G_PRIORITY_DEFAULT, NULL,
                                 simul_async_read_complete, test);
    }
}

static void
simul_async_write_complete (GObject      *object,
                            GAsyncResult *result,
                            gpointer      user_data)
{
  TestConnection *test = user_data;
  gssize nwrote;
  GError *error = NULL;

  nwrote = g_output_stream_write_finish (G_OUTPUT_STREAM (object),
                                         result, &error);
  g_assert_no_error (error);

  test->nwrote += nwrote;
  if (test->nwrote < TEST_DATA_LENGTH)
    {
      g_output_stream_write_async (G_OUTPUT_STREAM (object),
                                   &TEST_DATA[test->nwrote],
                                   TEST_DATA_LENGTH - test->nwrote,
                                   G_PRIORITY_DEFAULT, NULL,
                                   simul_async_write_complete, test);
    }
}

static void
test_simultaneous_async (TestConnection *test,
                         gconstpointer   data)
{
  GIOStream *connection;
  GTlsCertificateFlags flags;
  GError *error = NULL;

  connection = start_echo_server_and_connect_to_it (test);
  test->client_connection = g_tls_client_connection_new (connection, test->identity, &error);
  g_assert_no_error (error);
  g_object_unref (connection);

  flags = G_TLS_CERTIFICATE_VALIDATE_ALL &
    ~(G_TLS_CERTIFICATE_UNKNOWN_CA | G_TLS_CERTIFICATE_BAD_IDENTITY);
  g_tls_client_connection_set_validation_flags (G_TLS_CLIENT_CONNECTION (test->client_connection),
                                                flags);

  memset (test->buf, 0, sizeof (test->buf));
  test->nread = test->nwrote = 0;

  g_input_stream_read_async (g_io_stream_get_input_stream (test->client_connection),
                             test->buf, TEST_DATA_LENGTH / 2,
                             G_PRIORITY_DEFAULT, NULL,
                             simul_async_read_complete, test);
  g_output_stream_write_async (g_io_stream_get_output_stream (test->client_connection),
                               TEST_DATA, TEST_DATA_LENGTH / 2,
                               G_PRIORITY_DEFAULT, NULL,
                               simul_async_write_complete, test);

  g_main_loop_run (test->loop);
  wait_until_server_finished (test);

  g_assert_cmpint (test->nread, ==, TEST_DATA_LENGTH);
  g_assert_cmpint (test->nwrote, ==, TEST_DATA_LENGTH);
  g_assert_cmpstr (test->buf, ==, TEST_DATA);
}

static void
test_simultaneous_async_rehandshake (TestConnection *test,
                                     gconstpointer   data)
{
#ifdef BACKEND_IS_OPENSSL
  g_test_skip ("this needs more research on openssl");
  return;
#endif

  test->rehandshake = TRUE;
  test_simultaneous_async (test, data);
}

static gpointer
simul_read_thread (gpointer user_data)
{
  TestConnection *test = user_data;
  GInputStream *istream = g_io_stream_get_input_stream (test->client_connection);
  GError *error = NULL;
  gssize nread;

  while (test->nread < TEST_DATA_LENGTH)
    {
      nread = g_input_stream_read (istream,
                                   test->buf + test->nread,
                                   MIN (TEST_DATA_LENGTH / 2, TEST_DATA_LENGTH - test->nread),
                                   NULL, &error);
      g_assert_no_error (error);

      test->nread += nread;
    }

  return NULL;
}

static gpointer
simul_write_thread (gpointer user_data)
{
  TestConnection *test = user_data;
  GOutputStream *ostream = g_io_stream_get_output_stream (test->client_connection);
  GError *error = NULL;
  gssize nwrote;

  while (test->nwrote < TEST_DATA_LENGTH)
    {
      nwrote = g_output_stream_write (ostream,
                                      &TEST_DATA[test->nwrote],
                                      MIN (TEST_DATA_LENGTH / 2, TEST_DATA_LENGTH - test->nwrote),
                                      NULL, &error);
      g_assert_no_error (error);

      test->nwrote += nwrote;
    }

  return NULL;
}

static void
test_simultaneous_sync (TestConnection *test,
                        gconstpointer   data)
{
  GIOStream *connection;
  GTlsCertificateFlags flags;
  GError *error = NULL;
  GThread *read_thread, *write_thread;

  connection = start_echo_server_and_connect_to_it (test);
  test->client_connection = g_tls_client_connection_new (connection, test->identity, &error);
  g_assert_no_error (error);
  g_object_unref (connection);

  flags = G_TLS_CERTIFICATE_VALIDATE_ALL &
    ~(G_TLS_CERTIFICATE_UNKNOWN_CA | G_TLS_CERTIFICATE_BAD_IDENTITY);
  g_tls_client_connection_set_validation_flags (G_TLS_CLIENT_CONNECTION (test->client_connection),
                                                flags);

  memset (test->buf, 0, sizeof (test->buf));
  test->nread = test->nwrote = 0;

  read_thread = g_thread_new ("reader", simul_read_thread, test);
  write_thread = g_thread_new ("writer", simul_write_thread, test);

  /* We need to run the main loop to get the GThreadedSocketService to
   * receive the connection and spawn the server thread.
   */
  while (!test->server_connection)
    g_main_context_iteration (NULL, FALSE);

  g_thread_join (write_thread);
  g_thread_join (read_thread);

  g_assert_cmpint (test->nread, ==, TEST_DATA_LENGTH);
  g_assert_cmpint (test->nwrote, ==, TEST_DATA_LENGTH);
  g_assert_cmpstr (test->buf, ==, TEST_DATA);

  g_io_stream_close (test->client_connection, NULL, &error);
  g_assert_no_error (error);
}

static void
test_simultaneous_sync_rehandshake (TestConnection *test,
                                    gconstpointer   data)
{
#ifdef BACKEND_IS_OPENSSL
  g_test_skip ("this needs more research on openssl");
  return;
#endif

  test->rehandshake = TRUE;
  test_simultaneous_sync (test, data);
}

static void
test_close_immediately (TestConnection *test,
                        gconstpointer   data)
{
  GIOStream *connection;
  GError *error = NULL;

  connection = start_async_server_and_connect_to_it (test, G_TLS_AUTHENTICATION_NONE);
  test->client_connection = g_tls_client_connection_new (connection, test->identity, &error);
  g_assert_no_error (error);
  g_object_unref (connection);

  /*
   * At this point the server won't get a chance to run. But regardless
   * closing should not wait on the server, trying to handshake or something.
   */
  g_io_stream_close (test->client_connection, NULL, &error);
  g_assert_no_error (error);
}

static void
close_server_connection_uncleanly (TestConnection *test)
{
  GIOStream *base_iostream;
  GError *error = NULL;

  /* Instead of closing the GTlsConnection itself, we'll directly close its
   * underlying output stream in order to ensure the TLS close notify is never
   * sent.
   */
  g_object_get (test->server_connection,
                "base-io-stream", &base_iostream,
                NULL);

  g_io_stream_close (base_iostream, NULL, &error);
  g_assert_no_error (error);

  test->server_running = FALSE;

  g_object_unref (base_iostream);
}

static void
test_unclean_close_by_server (TestConnection *test,
                              gconstpointer   data)
{
  GSocketClient *client;
  GTlsCertificateFlags flags;
  GTlsConnection *client_connection;
  gssize nread;

#ifdef BACKEND_IS_OPENSSL
  g_test_skip ("this new test does not work with openssl, more research needed");
  return;
#endif

  start_async_server_service (test, G_TLS_AUTHENTICATION_NONE, HANDSHAKE_ONLY);
  client = g_socket_client_new ();
  g_socket_client_set_tls (client, TRUE);
  flags = G_TLS_CERTIFICATE_VALIDATE_ALL & ~G_TLS_CERTIFICATE_UNKNOWN_CA;
  /* test->address doesn't match the server's cert */
  flags = flags & ~G_TLS_CERTIFICATE_BAD_IDENTITY;
  g_socket_client_set_tls_validation_flags (client, flags);

  g_socket_client_connect_async (client, G_SOCKET_CONNECTABLE (test->address),
                                 NULL, socket_client_connected, test);
  g_main_loop_run (test->loop);

  /* The server might not have completed its handshake yet. We want to
   * wait until the handshake has completed successfully before closing
   * the connection.
   */
  while (!test->server_ever_handshaked)
    g_main_context_iteration (test->context, TRUE);

  close_server_connection_uncleanly (test);

  /* Because the server closed its connection uncleanly, we should receive
   * G_TLS_ERROR_EOF to warn that the close notify alert was not received,
   * indicating a truncation attack. The only other acceptable error here
   * is connection closed, which is an uncommon race.
   */
  nread = g_input_stream_read (g_io_stream_get_input_stream (test->client_connection),
                               test->buf, TEST_DATA_LENGTH,
                               NULL, &test->read_error);
  if (!g_error_matches (test->read_error, G_IO_ERROR, G_IO_ERROR_BROKEN_PIPE))
    g_assert_error (test->read_error, G_TLS_ERROR, G_TLS_ERROR_EOF);
  g_assert_cmpint (nread, ==, -1);

  /* Now do it again, except this time, we ignore truncation attacks by
   * disabling require_close_notify.
   */
  g_clear_error (&test->read_error);
  g_clear_object (&test->service);
  g_clear_object (&test->server_connection);
  test->server_ever_handshaked = FALSE;
  start_async_server_service (test, G_TLS_AUTHENTICATION_NONE, HANDSHAKE_ONLY);

  g_socket_client_set_tls (client, TRUE);
  g_socket_client_connect_async (client, G_SOCKET_CONNECTABLE (test->address),
                                 NULL, socket_client_connected, test);
  g_main_loop_run (test->loop);

  while (!test->server_ever_handshaked)
    g_main_context_iteration (test->context, TRUE);

  close_server_connection_uncleanly (test);

  client_connection = G_TLS_CONNECTION (g_tcp_wrapper_connection_get_base_io_stream (G_TCP_WRAPPER_CONNECTION (test->client_connection)));
  g_tls_connection_set_require_close_notify (client_connection, FALSE);

  nread = g_input_stream_read (g_io_stream_get_input_stream (test->client_connection),
                               test->buf, TEST_DATA_LENGTH,
                               NULL, &test->read_error);
  if (!g_error_matches (test->read_error, G_IO_ERROR, G_IO_ERROR_BROKEN_PIPE))
    g_assert_no_error (test->read_error);
  g_assert_cmpint (nread, ==, 0);

  g_object_unref (client);
}

static gboolean
async_implicit_handshake_dispatch (GPollableInputStream *stream,
                                   gpointer user_data)
{
  TestConnection *test = user_data;
  GError *error = NULL;
  gchar buffer[TEST_DATA_LENGTH];
  gssize size;
  gboolean keep_running;

  size = g_pollable_input_stream_read_nonblocking (stream, buffer,
                                                   TEST_DATA_LENGTH,
                                                   NULL, &error);

  keep_running = (-1 == size);

  if (keep_running)
    {
      g_assert_error (error, G_IO_ERROR, G_IO_ERROR_WOULD_BLOCK);
      g_error_free (error);
    }
  else
    {
      g_assert_no_error (error);
      g_assert_cmpint (size, ==, TEST_DATA_LENGTH);
      g_main_loop_quit (test->loop);
    }

  return keep_running;
}

static void
test_async_implicit_handshake (TestConnection *test, gconstpointer   data)
{
  GTlsCertificateFlags flags;
  GIOStream *stream;
  GInputStream *input_stream;
  GSource *input_source;
  GError *error = NULL;

  g_test_bug ("710691");

  stream = start_async_server_and_connect_to_it (test, G_TLS_AUTHENTICATION_NONE);
  test->client_connection = g_tls_client_connection_new (stream, test->identity, &error);
  g_assert_no_error (error);
  g_object_unref (stream);

  flags = G_TLS_CERTIFICATE_VALIDATE_ALL &
    ~(G_TLS_CERTIFICATE_UNKNOWN_CA | G_TLS_CERTIFICATE_BAD_IDENTITY);
  g_tls_client_connection_set_validation_flags (G_TLS_CLIENT_CONNECTION (test->client_connection),
                                                flags);

  /**
   * Create a source from the client's input stream. The dispatch
   * callback will be called a first time, which will perform a
   * non-blocking read triggering the asynchronous implicit
   * handshaking.
   */
  input_stream = g_io_stream_get_input_stream (test->client_connection);
  input_source =
    g_pollable_input_stream_create_source (G_POLLABLE_INPUT_STREAM (input_stream),
                                           NULL);

  g_source_set_callback (input_source,
                         (GSourceFunc) async_implicit_handshake_dispatch,
                         test, NULL);

  g_source_attach (input_source, NULL);
  g_source_unref (input_source);

  g_main_loop_run (test->loop);
  wait_until_server_finished (test);

  g_io_stream_close (G_IO_STREAM (test->client_connection), NULL, &error);
  g_assert_no_error (error);
  g_object_unref (test->client_connection);
  test->client_connection = NULL;
}

static void
handshake_completed (GObject      *object,
                     GAsyncResult *result,
                     gpointer      user_data)
{
  gboolean *complete = user_data;

  *complete = TRUE;
  return;
}

static void
test_output_stream_close (TestConnection *test,
                          gconstpointer   data)
{
  GIOStream *connection;
  GError *error = NULL;
  gboolean ret;
  gboolean handshake_complete = FALSE;
  gssize size;

#ifdef BACKEND_IS_OPENSSL
# if OPENSSL_VERSION_NUMBER >= 0x10101000L
  /* FIXME: This test fails most of the times with openssl 1.1.1, my guess is that
   * there is still some threading issue and we endup calling input_stream_read
   * from different threads and the same time.
   */
  g_test_skip ("this is not supported with openssl 1.1.1");
  return;
# endif
#endif

  connection = start_async_server_and_connect_to_it (test, G_TLS_AUTHENTICATION_NONE);
  test->client_connection = g_tls_client_connection_new (connection, test->identity, &error);
  g_assert_no_error (error);
  g_object_unref (connection);

  /* No validation at all in this test */
  g_tls_client_connection_set_validation_flags (G_TLS_CLIENT_CONNECTION (test->client_connection),
                                                0);

  g_tls_connection_handshake_async (G_TLS_CONNECTION (test->client_connection),
                                    G_PRIORITY_DEFAULT, NULL,
                                    handshake_completed, &handshake_complete);

  while (!handshake_complete)
    g_main_context_iteration (NULL, TRUE);

  ret = g_output_stream_close (g_io_stream_get_output_stream (test->client_connection),
                               NULL, &error);
  g_assert_no_error (error);
  g_assert_true (ret);

  /* Verify that double close returns TRUE */
  ret = g_output_stream_close (g_io_stream_get_output_stream (test->client_connection),
                               NULL, &error);
  g_assert_no_error (error);
  g_assert_true (ret);

  size = g_output_stream_write (g_io_stream_get_output_stream (test->client_connection),
                                "data", 4, NULL, &error);
  g_assert_cmpint (size, ==, -1);
  g_assert_error (error, G_IO_ERROR, G_IO_ERROR_CLOSED);
  g_clear_error (&error);

  /* We closed the output stream, but not the input stream, so receiving
   * data should still work.
   */
  read_test_data_async (test);
  g_main_loop_run (test->loop);
  wait_until_server_finished (test);

  g_assert_no_error (test->read_error);
  g_assert_no_error (test->server_error);

  ret = g_io_stream_close (test->client_connection, NULL, &error);
  g_assert_no_error (error);
  g_assert_true (ret);
}

static void
test_garbage_database (TestConnection *test,
                       gconstpointer   data)
{
  GIOStream *connection;
  GError *error = NULL;

#ifdef BACKEND_IS_OPENSSL
  g_test_skip ("this is not yet passing with openssl");
  return;
#endif

  test->database = g_tls_file_database_new (tls_test_file_path ("garbage.pem"), &error);
  g_assert_no_error (error);
  g_assert_nonnull (test->database);

  connection = start_async_server_and_connect_to_it (test, G_TLS_AUTHENTICATION_NONE);
  test->client_connection = g_tls_client_connection_new (connection, test->identity, &error);
  g_assert_no_error (error);
  g_assert_nonnull (test->client_connection);
  g_object_unref (connection);

  g_tls_connection_set_database (G_TLS_CONNECTION (test->client_connection), test->database);

  /* All validation in this test */
  g_tls_client_connection_set_validation_flags (G_TLS_CLIENT_CONNECTION (test->client_connection),
                                                G_TLS_CERTIFICATE_VALIDATE_ALL);

  read_test_data_async (test);
  g_main_loop_run (test->loop);
  wait_until_server_finished (test);

  /* Should reject the server's certificate, because our TLS database contains
   * no valid certificates.
   */
  g_assert_error (test->read_error, G_TLS_ERROR, G_TLS_ERROR_BAD_CERTIFICATE);
  g_assert_error (test->server_error, G_TLS_ERROR, G_TLS_ERROR_NOT_TLS);
}

static void
test_readwrite_after_connection_destroyed (TestConnection *test,
                                           gconstpointer   data)
{
  GIOStream *connection;
  GOutputStream *ostream;
  GInputStream *istream;
  unsigned char buffer[1];
  GError *error = NULL;

  g_test_bug ("792219");

  connection = start_async_server_and_connect_to_it (test, G_TLS_AUTHENTICATION_NONE);
  test->client_connection = g_tls_client_connection_new (connection, test->identity, &error);
  g_assert_no_error (error);
  g_object_unref (connection);

  istream = g_object_ref (g_io_stream_get_input_stream (test->client_connection));
  ostream = g_object_ref (g_io_stream_get_output_stream (test->client_connection));
  g_clear_object (&test->client_connection);

  /* The GTlsConnection has been destroyed, but its underlying streams
   * live on, because we have reffed them. Verify that attempts to read
   * and write produce only nice GErrors.
   */
  g_input_stream_read (istream, buffer, sizeof (buffer), NULL, &error);
  g_assert_error (error, G_IO_ERROR, G_IO_ERROR_CLOSED);
  g_clear_error (&error);

  g_output_stream_write (ostream, TEST_DATA, TEST_DATA_LENGTH,
                         G_PRIORITY_DEFAULT, &error);
  g_assert_error (error, G_IO_ERROR, G_IO_ERROR_CLOSED);
  g_clear_error (&error);

  g_input_stream_close (istream, NULL, &error);
  g_assert_no_error (error);

  g_output_stream_close (ostream, NULL, &error);
  g_assert_no_error (error);

  g_object_unref (istream);
  g_object_unref (ostream);
}

static void
test_alpn (TestConnection *test,
           const char * const *client_protocols,
           const char * const *server_protocols,
           const char *negotiated_protocol)
{
  GIOStream *connection;
  GError *error = NULL;

#ifdef BACKEND_IS_OPENSSL
  g_test_skip ("this is not yet passing with openssl");
  return;
#endif

  test->server_protocols = server_protocols;

  test->database = g_tls_file_database_new (tls_test_file_path ("ca-roots.pem"), &error);
  g_assert_no_error (error);
  g_assert_nonnull (test->database);

  connection = start_async_server_and_connect_to_it (test, G_TLS_AUTHENTICATION_NONE);
  test->client_connection = g_tls_client_connection_new (connection, test->identity, &error);
  g_assert_no_error (error);
  g_object_unref (connection);

  if (client_protocols)
    {
      g_tls_connection_set_advertised_protocols (G_TLS_CONNECTION (test->client_connection),
                                                 client_protocols);
    }

  g_tls_connection_set_database (G_TLS_CONNECTION (test->client_connection), test->database);

  read_test_data_async (test);
  g_main_loop_run (test->loop);
  wait_until_server_finished (test);

  g_assert_no_error (test->read_error);
  g_assert_no_error (test->server_error);

  g_assert_cmpstr (g_tls_connection_get_negotiated_protocol (G_TLS_CONNECTION (test->server_connection)), ==, negotiated_protocol);
  g_assert_cmpstr (g_tls_connection_get_negotiated_protocol (G_TLS_CONNECTION (test->client_connection)), ==, negotiated_protocol);
}

static void
test_alpn_match (TestConnection *test,
                 gconstpointer   data)
{
  const char * const client_protocols[] = { "one", "two", "three", NULL };
  const char * const server_protocols[] = { "four", "seven", "nine", "two", NULL };

  test_alpn (test, client_protocols, server_protocols, "two");
}

static void
test_alpn_no_match (TestConnection *test,
                    gconstpointer   data)
{
  const char * const client_protocols[] = { "one", "two", "three", NULL };
  const char * const server_protocols[] = { "four", "seven", "nine", NULL };

  test_alpn (test, client_protocols, server_protocols, NULL);
}

static void
test_alpn_client_only (TestConnection *test,
                       gconstpointer   data)
{
  const char * const client_protocols[] = { "one", "two", "three", NULL };

  test_alpn (test, client_protocols, NULL, NULL);
}

static void
test_alpn_server_only (TestConnection *test,
                       gconstpointer   data)
{
  const char * const server_protocols[] = { "four", "seven", "nine", "two", NULL };

  test_alpn (test, NULL, server_protocols, NULL);
}

static gboolean
on_accept_certificate_with_sync_close (GTlsClientConnection *conn,
                                       GTlsCertificate      *cert,
                                       GTlsCertificateFlags  errors,
                                       gpointer              user_data)
{
  GError *error = NULL;

  /* Attempting to perform a sync operation that would block the
   * handshake should fail, not deadlock.
   */
  g_io_stream_close (G_IO_STREAM (conn), NULL, &error);
  g_assert_error (error, G_IO_ERROR, G_IO_ERROR_FAILED);
  g_error_free (error);

  /* FIXME: When writing this test, I initially wanted to return FALSE
   * here to reject the connection. However, this surfaces a bug that I
   * have not fixed yet. The problem is the server is not seeing the end
   * of its g_output_stream_write() when the client fails the handshake.
   * No good. The server's implicit handshake failure should trigger a
   * write failure as well, instead of stalling. This needs to be fixed.
   *
   * Fixing this would allow us to guarantee that this callback is
   * actually executed by checking test->read_error at the bottom of
   * test_sync_op_during_handshake(). Currently, this test would still
   * pass even if this callback were to be improperly skipped.
   */
  return TRUE;
}

static void
test_sync_op_during_handshake (TestConnection *test,
                               gconstpointer   data)
{
  GIOStream *connection;
  GError *error = NULL;

#ifdef BACKEND_IS_OPENSSL
  g_test_skip ("this is not yet passing with openssl");
  return;
#endif

  connection = start_async_server_and_connect_to_it (test, G_TLS_AUTHENTICATION_NONE);
  test->client_connection = g_tls_client_connection_new (connection, test->identity, &error);
  g_assert_no_error (error);
  g_object_unref (connection);

  /* For this test, we need validation to fail to ensure that the
   * accept-certificate signal gets emitted.
   */
  g_tls_client_connection_set_validation_flags (G_TLS_CLIENT_CONNECTION (test->client_connection),
                                                G_TLS_CERTIFICATE_VALIDATE_ALL);

  g_signal_connect (test->client_connection, "accept-certificate",
                    G_CALLBACK (on_accept_certificate_with_sync_close), test);

  read_test_data_async (test);
  g_main_loop_run (test->loop);
  wait_until_server_finished (test);

  g_assert_no_error (test->read_error);
  g_assert_no_error (test->server_error);
}

static void
test_socket_timeout (TestConnection *test,
                     gconstpointer   data)
{
  GIOStream *connection;
  GSocketClient *client;
  GError *error = NULL;

#ifdef BACKEND_IS_OPENSSL
  g_test_skip ("this new test does not work with openssl, more research needed");
  return;
#endif

  test->incoming_connection_delay = (gulong)(1.1 * G_USEC_PER_SEC);

  start_async_server_service (test, G_TLS_AUTHENTICATION_NONE, WRITE_THEN_CLOSE);

  client = g_socket_client_new ();
  g_socket_client_set_timeout (client, 1);
  connection = G_IO_STREAM (g_socket_client_connect (client, G_SOCKET_CONNECTABLE (test->address),
                                                     NULL, &error));
  g_assert_no_error (error);
  g_object_unref (client);

  test->client_connection = g_tls_client_connection_new (connection, test->identity, &error);
  g_assert_no_error (error);
  g_object_unref (connection);

  /* No validation at all in this test */
  g_tls_client_connection_set_validation_flags (G_TLS_CLIENT_CONNECTION (test->client_connection),
                                                0);

  read_test_data_async (test);
  g_main_loop_run (test->loop);
  wait_until_server_finished (test);

  g_assert_error (test->read_error, G_IO_ERROR, G_IO_ERROR_TIMED_OUT);
  g_assert_error (test->server_error, G_TLS_ERROR, G_TLS_ERROR_NOT_TLS);
}

int
main (int   argc,
      char *argv[])
{
  int ret;

  g_test_init (&argc, &argv, NULL);
  g_test_bug_base ("http://bugzilla.gnome.org/");

  g_setenv ("GSETTINGS_BACKEND", "memory", TRUE);
  g_setenv ("GIO_USE_TLS", BACKEND, TRUE);

  g_assert_true (g_ascii_strcasecmp (G_OBJECT_TYPE_NAME (g_tls_backend_get_default ()), "GTlsBackend" BACKEND) == 0);

  g_test_add ("/tls/" BACKEND "/connection/basic", TestConnection, NULL,
              setup_connection, test_basic_connection, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/verified", TestConnection, NULL,
              setup_connection, test_verified_connection, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/verified-chain", TestConnection, NULL,
              setup_connection, test_verified_chain, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/verified-chain-with-redundant-root-cert", TestConnection, NULL,
              setup_connection, test_verified_chain_with_redundant_root_cert, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/verified-chain-with-duplicate-server-cert", TestConnection, NULL,
              setup_connection, test_verified_chain_with_duplicate_server_cert, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/verified-unordered-chain", TestConnection, NULL,
              setup_connection, test_verified_unordered_chain, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/verified-chain-with-alternative-ca-cert", TestConnection, NULL,
              setup_connection, test_verified_chain_with_alternative_ca_cert, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/invalid-chain-with-alternative-ca-cert", TestConnection, NULL,
              setup_connection, test_invalid_chain_with_alternative_ca_cert, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/client-auth", TestConnection, NULL,
              setup_connection, test_client_auth_connection, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/client-auth-rehandshake", TestConnection, NULL,
              setup_connection, test_client_auth_rehandshake, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/client-auth-failure", TestConnection, NULL,
              setup_connection, test_client_auth_failure, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/client-auth-fail-missing-client-private-key", TestConnection, NULL,
              setup_connection, test_client_auth_fail_missing_client_private_key, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/client-auth-request-cert", TestConnection, NULL,
              setup_connection, test_client_auth_request_cert, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/client-auth-request-fail", TestConnection, NULL,
              setup_connection, test_client_auth_request_fail, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/client-auth-request-none", TestConnection, NULL,
              setup_connection, test_client_auth_request_none, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/no-database", TestConnection, NULL,
              setup_connection, test_connection_no_database, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/failed", TestConnection, NULL,
              setup_connection, test_failed_connection, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/socket-client", TestConnection, NULL,
              setup_connection, test_connection_socket_client, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/socket-client-failed", TestConnection, NULL,
              setup_connection, test_connection_socket_client_failed, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/read-time-out-then-write", TestConnection, NULL,
              setup_connection, test_connection_read_time_out_write, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/simultaneous-async", TestConnection, NULL,
              setup_connection, test_simultaneous_async, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/simultaneous-sync", TestConnection, NULL,
              setup_connection, test_simultaneous_sync, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/simultaneous-async-rehandshake", TestConnection, NULL,
              setup_connection, test_simultaneous_async_rehandshake, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/simultaneous-sync-rehandshake", TestConnection, NULL,
              setup_connection, test_simultaneous_sync_rehandshake, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/close-immediately", TestConnection, NULL,
              setup_connection, test_close_immediately, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/unclean-close-by-server", TestConnection, NULL,
              setup_connection, test_unclean_close_by_server, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/async-implicit-handshake", TestConnection, NULL,
              setup_connection, test_async_implicit_handshake, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/output-stream-close", TestConnection, NULL,
              setup_connection, test_output_stream_close, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/garbage-database", TestConnection, NULL,
              setup_connection, test_garbage_database, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/readwrite-after-connection-destroyed", TestConnection, NULL,
              setup_connection, test_readwrite_after_connection_destroyed, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/alpn/match", TestConnection, NULL,
              setup_connection, test_alpn_match, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/alpn/no-match", TestConnection, NULL,
              setup_connection, test_alpn_no_match, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/alpn/client-only", TestConnection, NULL,
              setup_connection, test_alpn_client_only, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/alpn/server-only", TestConnection, NULL,
              setup_connection, test_alpn_server_only, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/sync-op-during-handshake", TestConnection, NULL,
              setup_connection, test_sync_op_during_handshake, teardown_connection);
  g_test_add ("/tls/" BACKEND "/connection/socket-timeout", TestConnection, NULL,
              setup_connection, test_socket_timeout, teardown_connection);

  ret = g_test_run ();

  /* for valgrinding */
  g_main_context_unref (g_main_context_default ());

  return ret;
}
