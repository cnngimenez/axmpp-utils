# Copyright 2019 Christian Gimenez

# Author: Christian Gimenez   

# Makefile

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


# Where to install the library and binaries?
PREFIX = /usr/local

# extra_projects= -aP ../axmpp/gnat
extra_projects =

all: compile install

compile: utils

install: uninstall
	gprinstall -p --prefix=$(PREFIX) -Paxmpp_utils.gpr

utils:
	gprbuild -p $(extra_projects) -Paxmpp_utils.gpr

clean:
	gprclean $(extra_projects) -Paxmpp_utils.gpr

.IGNORE: uninstall
uninstall:
	gprinstall --uninstall --prefix=$(PREFIX) -Paxmpp_utils.gpr
