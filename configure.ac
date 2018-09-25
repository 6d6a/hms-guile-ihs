AC_INIT([Guile-GMS], [0.0.1], [go.wigust@gmail.com], [guile-gms],
  [https://gms-billing.majordomo.ru/])

PKG_CHECK_MODULES([GUILE], [guile-2.2])
GUILE_PKG([2.2])
GUILE_PROGS
AC_ARG_WITH([guile-site-dir],
  [AS_HELP_STRING([--with-guile-site-dir],
    [installation directory for Guile (*.scm) files])],
  [guilemoduledir="$withval"],
  [guilemoduledir="$datarootdir/guile/site/$GUILE_EFFECTIVE_VERSION"])
AC_SUBST([guilemoduledir])
AC_MSG_CHECKING([for the Guile module directory])
AC_MSG_RESULT([$guilemoduledir])

AC_ARG_WITH([guile-site-ccache-dir],
  [AS_HELP_STRING([--with-guile-site-ccache-dir],
    [installation directory for Guile compiled (*.go) files])],
  [guileccachedir="$withval"],
  [guileccachedir="$libdir/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"])
AC_SUBST([guileccachedir])
AC_MSG_CHECKING([for the Guile ccache directory])
AC_MSG_RESULT([$guileccachedir])

AC_ARG_WITH([guix-site-dir],
  [AS_HELP_STRING([--with-guix-site-dir],
    [directory with Guix modules (*.scm files)])],
  [guixmoduledir="$withval"],
  [guixmoduledir=""])
AC_SUBST([guixmoduledir])

AC_ARG_WITH([guix-site-ccache-dir],
  [AS_HELP_STRING([--with-guix-site-ccache-dir],
    [directory with Guix compiled (*.go) files])],
  [guixccachedir="$withval"],
  [guixccachedir=""])

dnl If ccache dir is not specified, set it to the module dir.
AS_IF([test "x$guixccachedir" = "x" -a \
            "x$guixmoduledir" != "x"],
      [guixccachedir="$guixmoduledir"])

AC_SUBST([guixccachedir])

AM_CONDITIONAL([GUIX_DIR], [test "x$guixmoduledir" != "x"])

AC_PROG_SED

AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_FILES([Makefile
                 gms/config.scm])

AC_CONFIG_FILES([pre-inst-env:build-aux/pre-inst-env.in],
  [chmod +x pre-inst-env])

AC_OUTPUT
