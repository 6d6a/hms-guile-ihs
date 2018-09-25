# Guile GMS --- GMS command-line interface.
# Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
#
# This file is part of Guile GMS.
#
# Guile GMS is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# Guile GMS is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Guile GMS.  If not, see <http://www.gnu.org/licenses/>.

bin_SCRIPTS = scripts/gms

# Handle substitution of fully-expanded Autoconf variables.
do_subst = $(SED)					\
  -e 's,[@]GUILE[@],$(GUILE),g'

scripts/gms: scripts/gms.in Makefile
	$(AM_V_at)rm -f $@ $@-t
	$(AM_V_at)$(MKDIR_P) "$(@D)"
	$(AM_V_GEN)$(do_subst) < "$(srcdir)/$@.in" > "$@-t"
	$(AM_V_at)chmod a+x,a-w "$@-t" && mv -f "$@-t" "$@"

AM_V_GUILEC = $(AM_V_GUILEC_$(V))
AM_V_GUILEC_ = $(AM_V_GUILEC_$(AM_DEFAULT_VERBOSITY))
AM_V_GUILEC_0 = @echo "  GUILEC     " $@;

MODULES =					\
  gms/ui.scm					\
  gms/scripts.scm				\
  gms/scripts/account.scm			\
  gms//scripts/account/history.scm			\
  gms/scripts/account/search.scm			\
  gms/scripts/server.scm			\
  gms/scripts/vm.scm				\
  gms.scm

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
