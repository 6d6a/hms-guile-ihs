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
