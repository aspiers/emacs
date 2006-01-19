# configure.ac --- configuration setup for XTLA

# Author:        Robert Widhopf-Fenk <hack@robf.de>

# Copyright (C) 2004

# XTLA is free software; you can redistribute it and/or modify
# it under the terms of the GNU Library General Public License as published
# by the Free Software Foundation; either version 2 of the License, or (at
# your option) any later version.

# XTLA is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Library General Public License for more details.

# You should have received a copy of the GNU Library General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

# Process this file with autoconf to produce a new configure script

m4_define([XTLA_PRE_INIT],
[ m4_define([XTLA_VERSION],
             m4_esyscmd([if which tla 2>&1 > /dev/null; then
                            tla logs -f | tail -1 | cut -d / -f 2 | xargs printf '%s'
                         else
                            baz logs -f | tail -1 | cut -d / -f 2 | xargs printf '%s'
                         fi
                         ]))
])

# XTLA_ARG_SUBST(VAR, OPTION, VAL, DESC[, DEFAULT[, ACTION]])
#
# Substitute the autoconf variable VAR to a value specified by the user
# option --with-OPTION[=VAL] (described by DESC), or with a DEFAULT value.
# If an additional ACTION is given, it is executed at the top of the
# ACTION-IF-FOUND part of AC_ARG_WITH.
# #### WARNING: pay attention to the quoting of ACTION if given !!!!!
AC_DEFUN([XTLA_ARG_SUBST],
[
  AC_SUBST([$1])
  AC_ARG_WITH([$2],
    AC_HELP_STRING([--with-][$2]ifelse($3, [], [], [=$3]),
      [$4]ifelse($5, [], [], [ [[[$5]]]])),
    [
      ifelse($6, [], [], $6)
      $1="${withval}"
    ],
    ifelse($5, [], [], [$1="$5"]))
])

# XTLA_COLON_TO_SPACE(VAR)
#
# Transforms a (possibly) colon separated list VAR into a space separated
# one. VAR needs not be quoted.
AC_DEFUN([XTLA_COLON_TO_SPACE],
[ case "$$1" in *:*)
    $1="`echo $$1 | sed -e 's/:/ /g'`";;
  esac ])

# --with-other-dir option
AC_DEFUN([XTLA_OTHERDIRS],
  [ XTLA_ARG_SUBST([OTHERDIRS], [other-dirs], [DIRS],
    [set other needed directories (a list of space or colon separated paths)],
    [],
    dnl #### WARNING: pay attention to the quoting of ACTION if given !!!!!
    [[ XTLA_COLON_TO_SPACE(withval) ]])])

# Find a (g)tar program and make sure it is GNU one. A failure is not fatal
# since tar is needed for non critical targets only.
AC_DEFUN([XTLA_PROG_GNU_TAR],
  [ AC_CHECK_PROGS(TAR, gtar tar)
    if test "x${TAR}" = "xtar" ; then
      AC_MSG_CHECKING([that tar is GNU tar])
      ${TAR} --version > /dev/null 2>&1 || TAR=
      if test "x${TAR}" = "x" ; then
        AC_MSG_RESULT(no)
      else
        AC_MSG_RESULT(yes)
      fi
    fi
    if test "x${TAR}" = "x" ; then
      AC_MSG_WARN([*** No GNU tar program found.])
      AC_MSG_WARN([*** Some targets will be unavailable.])
    fi ])

# Find a makeinfo program. A failure is not fatal, only info files won't be
# built.
AC_DEFUN([XTLA_PROG_MAKEINFO],
  [ AC_CHECK_PROG(MAKEINFO, makeinfo, makeinfo)
    if test "x${MAKEINFO}" = "x" ; then
      AC_MSG_WARN([*** No makeinfo program found.])
      AC_MSG_WARN([*** Info files will not be built.])
    fi ])

# Find a texi2dvi program. A failure is not fatal, only dvi and pdf files
# won't be built.
AC_DEFUN([XTLA_PROG_TEXI2DVI],
  [ AC_CHECK_PROG(TEXI2DVI, texi2dvi, texi2dvi)
    if test "x${TEXI2DVI}" = "x" ; then
      AC_MSG_WARN([*** No texi2dvi program found.])
      AC_MSG_WARN([*** DVI and PDF files will not be built.])
    fi ])

# Find an etags program. A failure is not fatal, only TAGS file won't be
# built.
AC_DEFUN([XTLA_PROG_ETAGS],
  [ AC_CHECK_PROG(ETAGS, etags, etags)
    if test "x${ETAGS}" = "x" ; then
      AC_MSG_WARN([*** No etags program found.])
      AC_MSG_WARN([*** Tags file will not be built.])
    fi ])

# Find a diff program. A failure is not fatal.
AC_DEFUN([XTLA_PROG],
  [ AC_SUBST([$1])
    AC_ARG_WITH([$2],
      AC_HELP_STRING([--with-][$2]=PROG, [$3]),
      [ [$1]=${withval} ],
      [
        AC_CHECK_PROG($1, $2, $2)
        if test "x${$1}" = "x" ; then
          AC_MSG_WARN([*** No $2 program found.])
        fi
      ])
  ])

# Find an xargs program. A failure is not fatal, only clean/tarball will not
# work
AC_DEFUN([XTLA_PROG_XARGS],
  [ AC_CHECK_PROG(XARGS, xargs, xargs)
    if test "x${XARGS}" = "x" ; then
      AC_MSG_WARN([*** No xargs program found.])
      AC_MSG_WARN([*** make clean/tarball will not work.])
    fi ])

AC_DEFUN([XTLA_BAZ_OR_TLA],
  [ AC_MSG_CHECKING([arch branch to use])
    AC_ARG_WITH(arch,
      AC_HELP_STRING([--with-arch={baz|tla}]
        [Whether to use tla or baz by default]),
      [ ARCH_BRANCH=${withval} ]
      [ ])
    if test "x${BAZ}" != "x"; then
      if test "x${ARCH_BRANCH}" = "x"; then
        ARCH_BRANCH=baz
      fi
      ARCH=${BAZ}
    else
      if test "x${ARCH_BRANCH}" = "x"; then
        ARCH_BRANCH=tla
      fi
      ARCH=${TLA}
    fi
    if test "x${ARCH}" = "x"; then
      AC_MSG_ERROR([*** Neither tla nor baz found])
    fi
    if test "x${ARCH_BRANCH}" = "xtla" -a "x${TLA}" = "x"; then
      AC_MSG_ERROR([*** Can't use tla branch because tla isn't available])
    fi
    if test "x${ARCH_BRANCH}" = "xbaz" -a "x${BAZ}" = "x"; then
      AC_MSG_ERROR([*** Can't use baz branch because baz isn't available])
    fi
    AC_MSG_RESULT(${ARCH_BRANCH})
    AC_SUBST(ARCH_BRANCH)
  ])


# Choose an Emacs flavor according to the --with-emacs user option, or try
# emacs and xemacs.
# We use EMACS_PROG instead of EMACS to avoid colliding with Emacs' own
# internal environment.
AC_DEFUN([XTLA_PROG_EMACS],
  [ AC_ARG_WITH([emacs],
     AC_HELP_STRING([--with-emacs=PROG],
        [choose which flavor of Emacs to use]),
      [ EMACS_PROG="${withval}" ],
      [ AC_CHECK_PROGS(EMACS_PROG, emacs xemacs) ])
    if test "x${EMACS_PROG}" = "x" ; then
      dnl This is critical enough to generate an error and not a warning...
      AC_MSG_ERROR([*** No Emacs program found.])
    fi
    # EMACS TYPE ##################################################
    AC_MSG_CHECKING([checking emacs-type of ${EMACS_PROG}])
    cat > conftest.el <<TEST
(princ (format "%s" (if (featurep 'xemacs ) 'xemacs 'emacs)))
TEST
    EMACS_FLAVOR=`${EMACS_PROG} --no-site-file --batch -l conftest.el | ${GREP} .`
    AC_MSG_RESULT([${EMACS_FLAVOR}])
    # EMACS VERSION ###############################################
    AC_MSG_CHECKING([checking emacs-version of ${EMACS_PROG}])
    cat > conftest.el <<TEST
(if (featurep 'xemacs )
     (princ (format "%d.%d.%d"
                 emacs-major-version emacs-minor-version emacs-patch-level))
  (princ (format "%s" emacs-version)))
TEST
    EMACS_VERSION=`${EMACS_PROG} --no-site-file --batch -l conftest.el | ${GREP} .`
    # EMACS DEPENDENT SETTINGS ####################################
    # We may add a version check here ...
    AC_MSG_RESULT([${EMACS_VERSION}])

    # Copied from gnus aclocal.m4
    AC_ARG_WITH(lispdir,[  --with-lispdir=DIR      Where to install lisp files], lispdir=${withval})
    AC_MSG_CHECKING([where .elc files should go])
    if test -z "$lispdir"; then
      dnl Set default value
      theprefix=$prefix
      if test "x$theprefix" = "xNONE"; then
  	theprefix=$ac_default_prefix
      fi
      datadir="\$(prefix)/share"
      if test "$EMACS_FLAVOR" = "xemacs"; then
          datadir="\$(prefix)/lib"
          lispdir="${datadir}/${EMACS_FLAVOR}/site-packages/lisp/xtla"
      else
          lispdir="${datadir}/${EMACS_FLAVOR}/site-lisp/xtla"
      fi
      for thedir in share lib; do
  	potential=
  	if test -d ${theprefix}/${thedir}/${EMACS_FLAVOR}/site-lisp; then
             if test "$EMACS_FLAVOR" = "xemacs"; then
  	         lispdir="\$(prefix)/${thedir}/${EMACS_FLAVOR}/site-packages/lisp/xtla"
             else
                 lispdir="${datadir}/${EMACS_FLAVOR}/site-lisp/xtla"
             fi
  	   break
  	fi
      done
    fi
    AC_MSG_RESULT($lispdir)
    AC_SUBST(lispdir)

#    if test "x${EMACS_FLAVOR}" = "xemacs" ; then
#      PACKAGEDIR="${prefix}/share/emacs/site-lisp"
#    else
#      PACKAGEDIR="${HOME}/.xemacs/xemacs-packages"
#    fi

    AC_SUBST(EMACS_PROG)
    AC_SUBST(EMACS_FLAVOR)
    AC_SUBST(EMACS_VERSION)
])

# Copied from gnus aclocal.m4
AC_DEFUN(AC_PATH_INFO_DIR, [
  AC_MSG_CHECKING([where the TeXinfo docs should go])
  dnl Set default value.  This must be an absolute path.
  if test "$infodir" = "\${prefix}/info"; then
    if test "$EMACS_FLAVOR" = "xemacs"; then
      info_dir="\$(prefix)/${thedir}/${EMACS_FLAVOR}/site-packages/info"
    else
      info_dir="\$(prefix)/share/info"
    fi
  else
    info_dir=$infodir
  fi
  AC_MSG_RESULT($info_dir)
  AC_SUBST(info_dir)
])

AC_DEFUN([XTLA_HAS_TREE_WIDGET],
  [ AC_MSG_CHECKING([checking wether tree-widget is in the load-path])
    OTHERDIRS_LIST='"'`echo ${OTHERDIRS} | sed 's/ /" "/g'`'"'
    cat > conftest.el << TEST
(mapcar (lambda (x) (add-to-list 'load-path x))
        '(${OTHERDIRS_LIST}))
(if (locate-library "tree-widget")
    (princ "yes")
    (princ "no"))
TEST
    HAS_TREE_WIDGET=`${EMACS_PROG} --no-site-file --batch -l conftest.el | ${GREP} .`
    AC_MSG_RESULT([${HAS_TREE_WIDGET}])
    if test "x${HAS_TREE_WIDGET}" = "xno" ; then
      AC_MSG_WARN([*** tree-widget.el not found in 'load-path.])
      AC_MSG_WARN([*** xtla-browse.el won't be available unless you install it.])
      AC_MSG_WARN([*** See http://sourceforge.net/projects/emhacks/])
      AC_MSG_WARN([*** Provide the path to tree-widget with --with-other-dirs])
      AC_MSG_WARN([*** if tree-widget.el is already present on your system])
    fi
    AC_SUBST(HAS_TREE_WIDGET)
  ])

dnl dnl AC_DEFUN([XTLA_RELEASE_ID],
dnl dnl         [ AC_MSG_CHECKING(for release ID)
dnl dnl           AC_ARG_WITH([release-id],
dnl dnl             AC_HELP_STRING([--with-release-id=ID],
dnl dnl             [Version number for an official release]),
dnl dnl             [ RELEASE_ID="${withval}";
dnl dnl               AC_MSG_RESULT(${RELEASE_ID})],
dnl dnl             [ RELEASE_ID="";
dnl dnl               AC_MSG_RESULT(none)])]
dnl dnl           AC_SUBST(RELEASE_ID))


##############################################################################
XTLA_PRE_INIT
AC_INIT([XTLA], [XTLA_VERSION], [xtla-el-dev@gna.org])

AC_COPYRIGHT([Copyright (C) 2004 Robert Widhopf-Fenk <hack@robf.de>])

AC_CONFIG_SRCDIR([configure.ac])
AC_CONFIG_FILES([Makefile lisp/Makefile texinfo/Makefile xtla-load.el xtla-load-install.el lisp/xtla-site.el])

# Common system utilities checking:
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PATH_PROG(RM, rm, /bin/rm)
AC_PATH_PROG(GREP, grep, /bin/grep)

# External programs checking:
XTLA_PROG_EMACS
AC_PATH_INFO_DIR


XTLA_PROG(TLA, tla, tla program to use)
XTLA_PROG(BAZ, baz, baz program to use)
XTLA_BAZ_OR_TLA

XTLA_PROG(DIFF, diff, diff program to use)
XTLA_PROG(PATCH, patch, patch program to use)
XTLA_PROG_ETAGS
XTLA_PROG_XARGS
XTLA_PROG_GNU_TAR
XTLA_PROG_MAKEINFO
XTLA_PROG_TEXI2DVI
XTLA_OTHERDIRS
XTLA_HAS_TREE_WIDGET
dnl dnl XTLA_RELEASE_ID

# is there a sane way to set this to a useful default?
XTLA_ARG_SUBST([PACKAGEDIR], [package-dir], [DIR],
  [set the Emacs package directory to DIR],)

XTLA_ARG_SUBST([SYMLINKS], [symlinks], [],
  [install XTLA by linking instead of copying], [no])

XTLA_ARG_SUBST([LINKPATH], [linkpath], [PATH],
  [path to symlink from if `pwd' does not work])

AC_OUTPUT

# configure.ac ends here
