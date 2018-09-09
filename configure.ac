AC_INIT([Guile-GMS], [0.0.1], [go.wigust@gmail.com], [guile-gms],
  [https://gms-billing.majordomo.ru/])

PKG_CHECK_MODULES([GUILE], [guile-2.2])
GUILE_PKG([2.2])
GUILE_PROGS

AC_PROG_SED

AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_FILES([Makefile])

AC_CONFIG_FILES([pre-inst-env:build-aux/pre-inst-env.in],
  [chmod +x pre-inst-env])

AC_OUTPUT
