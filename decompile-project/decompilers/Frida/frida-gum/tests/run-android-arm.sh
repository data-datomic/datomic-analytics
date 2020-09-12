#!/bin/sh

arch=arm
apex_libdirs="/apex/com.android.art/lib:/apex/com.android.runtime/lib"

remote_prefix=/data/local/tmp/frida-tests-$arch

gum_tests=$(dirname "$0")
cd "$gum_tests/../../build/tmp-android-$arch/frida-gum" || exit 1
. ../../frida-meson-env-macos-x86_64.rc
ninja || exit 1
cd tests
adb shell "mkdir $remote_prefix"
adb push gum-tests data $remote_prefix || exit 1
adb shell "LD_LIBRARY_PATH='$apex_libdirs' $remote_prefix/gum-tests $@"
