# Guile IHS --- IHS command-line interface.
# Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
#
# This file is part of Guile IHS.
#
# Guile IHS is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# Guile IHS is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Guile IHS.  If not, see <http://www.gnu.org/licenses/>.

bin_SCRIPTS = scripts/ihs

# Handle substitution of fully-expanded Autoconf variables.
do_subst = $(SED)					\
  -e 's,[@]GUILE[@],$(GUILE),g'

scripts/ihs: scripts/ihs.in Makefile
	$(AM_V_at)rm -f $@ $@-t
	$(AM_V_at)$(MKDIR_P) "$(@D)"
	$(AM_V_GEN)$(do_subst) < "$(srcdir)/$@.in" > "$@-t"
	$(AM_V_at)chmod a+x,a-w "$@-t" && mv -f "$@-t" "$@"

AM_V_GUILEC = $(AM_V_GUILEC_$(V))
AM_V_GUILEC_ = $(AM_V_GUILEC_$(AM_DEFAULT_VERBOSITY))
AM_V_GUILEC_0 = @echo "  GUILEC     " $@;

MODULES =					\
  ihs/ui.scm					\
  ihs/scripts.scm				\
  ihs/scripts/web.scm				\
  ihs/scripts/web/history.scm			\
  ihs/scripts/web/panel.scm			\
  ihs/scripts/web/search.scm			\
  ihs/scripts/cerb.scm				\
  ihs/scripts/vm.scm				\
  ihs/utils.scm					\
  ihs.scm

GO_FILES = $(MODULES:%.scm=%.go)

nobase_dist_guilemodule_DATA = $(MODULES)
nobase_nodist_guileccache_DATA = $(GO_FILES)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>.
guile_install_go_files = install-nobase_nodist_guileccacheDATA
$(guile_install_go_files): install-nobase_dist_guilemoduleDATA

GUILEC_ENV =					\
  GUILE_AUTO_COMPILE=0

GUILEC_OPTS =					\
  -Warity-mismatch				\
  -Wformat					\
  -Wunbound-variable

if GUIX_DIR
  guix_module_part=":$(guixmoduledir)"
  guix_ccache_part=":$(guixccachedir)"
else
  guix_module_part=
  guix_ccache_part=
endif

# Guile PATHs shouldn't be unset: some guix modules want to load
# (gnutls) module, that's why 'guix' package propagates 'gnutls', i.e.
# a directory with gnutls module is placed in GUILE_LOAD_PATH.

GUILEC_ENV +=										   \
  GUILE_LOAD_PATH="$(abs_srcdir)$(guix_module_part):$$GUILE_LOAD_PATH"			   \
  GUILE_LOAD_COMPILED_PATH="$(abs_builddir)$(guix_ccache_part):$$GUILE_LOAD_COMPILED_PATH"

$(GO_FILES): %.go: %.scm
	-$(AM_V_GUILEC) $(GUILEC_ENV) \
	$(GUILD) compile $(GUILEC_OPTS) --output=$@ $<

# Test extensions; has to be unconditional.
TEST_EXTENSIONS = .scm

SCM_TESTS =					\
  tests/web.scm			\
  tests/search.scm			\
  tests/server.scm			\
  tests/vm.scm

TESTS = $(SCM_TESTS)

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)" GUILE_AUTO_COMPILE=0

SCM_LOG_DRIVER =				\
  $(top_builddir)/pre-inst-env			\
  $(GUILE) --no-auto-compile -e main		\
      $(top_srcdir)/build-aux/test-driver.scm

AM_SCM_LOG_DRIVER_FLAGS = --brief=yes
