#!/bin/bash
#
# vim: set ts=4 sw=4 et:
#
# Passed arguments:
#	$1 - pkgname [REQUIRED]
#   $2 - subpkg mode [REQUIRED]
#	$2 - cross target [OPTIONAL]

if [ $# -lt 2 -o $# -gt 3 ]; then
    echo "${0##*/}: invalid number of arguments: pkgname subpkg-mode [cross-target]"
    exit 1
fi

PKGNAME="$1"
SUBPKG_MODE="$2"
XBPS_CROSS_BUILD="$3"

for f in $XBPS_SHUTILSDIR/*.sh; do
    . $f
done

setup_pkg "$PKGNAME" $XBPS_CROSS_BUILD

for f in $XBPS_COMMONDIR/environment/install/*.sh; do
    source_file "$f"
done

XBPS_INSTALL_DONE="${XBPS_STATEDIR}/${sourcepkg}_${XBPS_CROSS_BUILD}_install_done"

cd "$wrksrc" || msg_error "$pkgver: cannot access to wrksrc [$wrksrc]\n"
if [ -n "$build_wrksrc" ]; then
    cd $build_wrksrc || msg_error "$pkgver: cannot access to build_wrksrc [$build_wrksrc]\n"
fi

if [ "$SUBPKG_MODE"  = "no" ]; then
    if [ ! -f $XBPS_INSTALL_DONE ] || [ -f $XBPS_INSTALL_DONE -a -n "$XBPS_BUILD_FORCEMODE" ]; then
        mkdir -p $XBPS_DESTDIR/$XBPS_CROSS_TRIPLET/$pkgname-$version

        # Run pre-install hooks
        run_pkg_hooks pre-install

        # Run pre_install()
        if declare -f pre_install >/dev/null; then
            run_func pre_install
        fi

        # Run do_install()
        cd "$wrksrc"
        [ -n "$build_wrksrc" ] && cd $build_wrksrc
        if declare -f do_install >/dev/null; then
            run_func do_install
        else
            if [ ! -r $XBPS_BUILDSTYLEDIR/${build_style}.sh ]; then
                msg_error "$pkgver: cannot find build helper $XBPS_BUILDSTYLEDIR/${build_style}.sh!\n"
            fi
            . $XBPS_BUILDSTYLEDIR/${build_style}.sh
            run_func do_install
        fi
        # Run post_install()
        cd "$wrksrc"
        [ -n "$build_wrksrc" ] && cd $build_wrksrc
        if declare -f post_install >/dev/null; then
            run_func post_install
        fi

        touch -f $XBPS_INSTALL_DONE
    fi
    exit 0
fi

XBPS_SUBPKG_INSTALL_DONE="${XBPS_STATEDIR}/${PKGNAME}_${XBPS_CROSS_BUILD}_subpkg_install_done"

# If it's a subpkg execute the pkg_install() function.
if [ ! -f $XBPS_SUBPKG_INSTALL_DONE ]; then
    if [ "$sourcepkg" != "$PKGNAME" ]; then
        # Source all subpkg environment setup snippets.
        for f in ${XBPS_COMMONDIR}/environment/setup-subpkg/*.sh; do
            source_file "$f"
        done

        ${PKGNAME}_package
        pkgname=$PKGNAME

        source_file $XBPS_COMMONDIR/environment/build-style/${build_style}.sh

        install -d $PKGDESTDIR
        if declare -f pkg_install >/dev/null; then
            export XBPS_PKGDESTDIR=1
            run_pkg_hooks pre-install
            run_func pkg_install
        fi
    fi
    setup_pkg_depends ${pkgname:=$PKGNAME}
    run_pkg_hooks post-install
    touch -f $XBPS_SUBPKG_INSTALL_DONE
fi

exit 0
