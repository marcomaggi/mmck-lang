## Process this file with automake to produce Makefile.in

include meta/automake/mmck-rules.make


#### documentation

EXTRA_DIST		+= doc/mmck-chicken-docs.style.css
CLEANFILES		+= doc/mmck-chicken-docs.css
AM_MAKEINFOHTMLFLAGS	+= --css-ref=mmck-chicken-docs.css

info_TEXINFOS		= doc/mmck-lang.texi
doc_mmck_lang_TEXINFOS = \
	doc/macros.texi			\
	doc/lgpl-3.0.texi		\
	doc/fdl-1.3.texi

## --------------------------------------------------------------------

doc/mmck-lang.html/$(am__dirstamp): doc/$(am__dirstamp)
	@$(MKDIR_P) doc/mmck-lang.html/
	@: > doc/mmck-lang.html/$(am__dirstamp)

doc/mmck-lang.html/mmck-chicken-docs.css: doc/mmck-chicken-docs.style.css doc/mmck-lang.html/$(am__dirstamp)
	$(INSTALL) -m 0444 "$(top_srcdir)/doc/mmck-chicken-docs.style.css" "$(builddir)/doc/mmck-lang.html/mmck-chicken-docs.css"

## --------------------------------------------------------------------

html-local: doc/mmck-lang.html/mmck-chicken-docs.css

install-html-local:
	$(MKDIR_P) "$(DESTDIR)$(htmldir)/mmck-lang.html/"
	$(INSTALL) -m 0444 $(builddir)/doc/mmck-lang.html/mmck-chicken-docs.css "$(DESTDIR)$(htmldir)/mmck-lang.html/"


#### library build targets

# All the source files ".scm" of the extension library.
#
MMCK_LANG_MODULES		=

# All  the object  files ".o"  that must  be linked  in the  extension
# library.
#
MMCK_LANG_RUNTIME_OBJECTS	=

# All  the  source files  ".import.scm"  of  all the  modules'  import
# libraries.
#
MMCK_LANG_IMPORT_SOURCES	=

# The  module's  import  libraries ".import.$(MMCK_SOEXT)"  needed  to
# compile main  module's object  file.  It does  not include  the main
# import library:
#
#   lib/mmck.lang.import.$(MMCK_SOEXT)
#
MMCK_LANG_IMPORT_DEPS		=

# The import  libraries ".import.$(MMCK_SOEXT)" needed to  compile new
# code using the  shared library "(mmck lang)".  We  need to build
# and install all of them.
#
MMCK_LANG_IMPORT_SHARED_LIBS	=

# This is the "(mmck lang)" shared library file pathname.
#
MMCK_LANG_RUNTIME_SHARED_LIBS	= \
	lib/mmck.lang.$(MMCK_SOEXT)

# Temporary  files  we need  for  parallel  builds, ".temp",  ".lock",
# ".stamp".
#
MMCK_LANG_TEMP_FILES		=


#### library (mmck lang): module version

MMCK_LANG_MODULES		+= lib/mmck.lang.version.scm
MMCK_LANG_RUNTIME_OBJECTS	+= lib/mmck.lang.version.$(OBJEXT)
MMCK_LANG_IMPORT_SOURCES	+= lib/mmck.lang.version.import.scm
MMCK_LANG_IMPORT_DEPS		+= lib/mmck.lang.version.import.$(MMCK_SOEXT)
MMCK_LANG_IMPORT_SHARED_LIBS	+= lib/mmck.lang.version.import.$(MMCK_SOEXT)
MMCK_LANG_TEMP_FILES		+= \
	lib/mmck.lang.version.temp				\
	lib/mmck.lang.version.lock				\
	lib/mmck.lang.version.stamp

# A compiler  invokation to build  an object file, also  generates the
# source  file for  the import  library; a  single rule  generates two
# targets.  We implement  the rules as suggested  in the documentation
# of GNU Automake, node "Multiple Outputs".
#
# It is ugly, I know.  But it  is supposed to support all the cases of
# parallel build and dry runs.  (Marco Maggi; May  4, 2019)
#
lib/mmck.lang.version.stamp: lib/mmck.lang.version.scm $(MMCK_OBJECTS_DEPS)
	@rm -f lib/mmck.lang.version.temp
	@touch lib/mmck.lang.version.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.lang.version.$(OBJEXT) $(<) -prologue lib/config.scm
	@$(MV) mmck.lang.version.import.scm lib/mmck.lang.version.import.scm
	@mv -f lib/mmck.lang.version.temp $@

lib/mmck.lang.version.import.scm lib/mmck.lang.version.$(OBJEXT): lib/mmck.lang.version.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.lang.version.lock lib/mmck.lang.version.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.lang.version.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.lang.version.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.lang.version.stamp; \
	    $$dry rmdir lib/mmck.lang.version.lock; \
	  else \
	    while test -d lib/mmck.lang.version.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.lang.version.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.lang.version.import.$(MMCK_SOEXT): lib/mmck.lang.version.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck lang): module debug

MMCK_LANG_MODULES		+= lib/mmck.lang.debug.scm
MMCK_LANG_RUNTIME_OBJECTS	+= lib/mmck.lang.debug.$(OBJEXT)
MMCK_LANG_IMPORT_SOURCES	+= lib/mmck.lang.debug.import.scm
MMCK_LANG_IMPORT_DEPS		+= lib/mmck.lang.debug.import.$(MMCK_SOEXT)
MMCK_LANG_IMPORT_SHARED_LIBS	+= lib/mmck.lang.debug.import.$(MMCK_SOEXT)
MMCK_LANG_TEMP_FILES		+= \
	lib/mmck.lang.debug.temp	\
	lib/mmck.lang.debug.lock	\
	lib/mmck.lang.debug.stamp

MMCK_LANG_DEBUG_DEPS = \
	$(MMCK_OBJECTS_DEPS)

lib/mmck.lang.debug.stamp: lib/mmck.lang.debug.scm $(MMCK_LANG_DEBUG_DEPS)
	@rm -f lib/mmck.lang.debug.temp
	@touch lib/mmck.lang.debug.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.lang.debug.$(OBJEXT) $(<)
	@$(MV) mmck.lang.debug.import.scm lib/mmck.lang.debug.import.scm
	@mv -f lib/mmck.lang.debug.temp $@

lib/mmck.lang.debug.import.scm lib/mmck.lang.debug.$(OBJEXT): lib/mmck.lang.debug.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.lang.debug.lock lib/mmck.lang.debug.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.lang.debug.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.lang.debug.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.lang.debug.stamp; \
	    $$dry rmdir lib/mmck.lang.debug.lock; \
	  else \
	    while test -d lib/mmck.lang.debug.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.lang.debug.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.lang.debug.import.$(MMCK_SOEXT): lib/mmck.lang.debug.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck lang): module core

MMCK_LANG_MODULES		+= lib/mmck.lang.core.scm
MMCK_LANG_RUNTIME_OBJECTS	+= lib/mmck.lang.core.$(OBJEXT)
MMCK_LANG_IMPORT_SOURCES	+= lib/mmck.lang.core.import.scm
MMCK_LANG_IMPORT_DEPS		+= lib/mmck.lang.core.import.$(MMCK_SOEXT)
MMCK_LANG_IMPORT_SHARED_LIBS	+= lib/mmck.lang.core.import.$(MMCK_SOEXT)
MMCK_LANG_TEMP_FILES		+= \
	lib/mmck.lang.core.temp	\
	lib/mmck.lang.core.lock	\
	lib/mmck.lang.core.stamp

MMCK_LANG_CORE_DEPS = \
	lib/mmck.lang.debug.import.$(MMCK_SOEXT)	\
	$(MMCK_OBJECTS_DEPS)

lib/mmck.lang.core.stamp: lib/mmck.lang.core.scm $(MMCK_LANG_CORE_DEPS)
	@rm -f lib/mmck.lang.core.temp
	@touch lib/mmck.lang.core.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.lang.core.$(OBJEXT) $(<)
	@$(MV) mmck.lang.core.import.scm lib/mmck.lang.core.import.scm
	@mv -f lib/mmck.lang.core.temp $@

lib/mmck.lang.core.import.scm lib/mmck.lang.core.$(OBJEXT): lib/mmck.lang.core.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.lang.core.lock lib/mmck.lang.core.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.lang.core.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.lang.core.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.lang.core.stamp; \
	    $$dry rmdir lib/mmck.lang.core.lock; \
	  else \
	    while test -d lib/mmck.lang.core.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.lang.core.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.lang.core.import.$(MMCK_SOEXT): lib/mmck.lang.core.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck lang): module booleans

MMCK_LANG_MODULES		+= lib/mmck.lang.booleans.scm
MMCK_LANG_RUNTIME_OBJECTS	+= lib/mmck.lang.booleans.$(OBJEXT)
MMCK_LANG_IMPORT_SOURCES	+= lib/mmck.lang.booleans.import.scm
MMCK_LANG_IMPORT_DEPS		+= lib/mmck.lang.booleans.import.$(MMCK_SOEXT)
MMCK_LANG_IMPORT_SHARED_LIBS	+= lib/mmck.lang.booleans.import.$(MMCK_SOEXT)
MMCK_LANG_TEMP_FILES		+= \
	lib/mmck.lang.booleans.temp	\
	lib/mmck.lang.booleans.lock	\
	lib/mmck.lang.booleans.stamp

MMCK_LANG_BOOLEANS_DEPS = \
	lib/mmck.lang.debug.import.$(MMCK_SOEXT)	\
	lib/mmck.lang.core.import.$(MMCK_SOEXT)		\
	$(MMCK_OBJECTS_DEPS)

lib/mmck.lang.booleans.stamp: lib/mmck.lang.booleans.scm $(MMCK_LANG_BOOLEANS_DEPS)
	@rm -f lib/mmck.lang.booleans.temp
	@touch lib/mmck.lang.booleans.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.lang.booleans.$(OBJEXT) $(<)
	@$(MV) mmck.lang.booleans.import.scm lib/mmck.lang.booleans.import.scm
	@mv -f lib/mmck.lang.booleans.temp $@

lib/mmck.lang.booleans.import.scm lib/mmck.lang.booleans.$(OBJEXT): lib/mmck.lang.booleans.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.lang.booleans.lock lib/mmck.lang.booleans.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.lang.booleans.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.lang.booleans.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.lang.booleans.stamp; \
	    $$dry rmdir lib/mmck.lang.booleans.lock; \
	  else \
	    while test -d lib/mmck.lang.booleans.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.lang.booleans.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.lang.booleans.import.$(MMCK_SOEXT): lib/mmck.lang.booleans.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck lang): module lists

MMCK_LANG_MODULES		+= lib/mmck.lang.lists.scm
MMCK_LANG_RUNTIME_OBJECTS	+= lib/mmck.lang.lists.$(OBJEXT)
MMCK_LANG_IMPORT_SOURCES	+= lib/mmck.lang.lists.import.scm
MMCK_LANG_IMPORT_DEPS		+= lib/mmck.lang.lists.import.$(MMCK_SOEXT)
MMCK_LANG_IMPORT_SHARED_LIBS	+= lib/mmck.lang.lists.import.$(MMCK_SOEXT)
MMCK_LANG_TEMP_FILES		+= \
	lib/mmck.lang.lists.temp	\
	lib/mmck.lang.lists.lock	\
	lib/mmck.lang.lists.stamp

MMCK_LANG_LISTS_DEPS = \
	lib/mmck.lang.debug.import.$(MMCK_SOEXT)	\
	lib/mmck.lang.core.import.$(MMCK_SOEXT)		\
	$(MMCK_OBJECTS_DEPS)

lib/mmck.lang.lists.stamp: lib/mmck.lang.lists.scm $(MMCK_LANG_LISTS_DEPS)
	@rm -f lib/mmck.lang.lists.temp
	@touch lib/mmck.lang.lists.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.lang.lists.$(OBJEXT) $(<)
	@$(MV) mmck.lang.lists.import.scm lib/mmck.lang.lists.import.scm
	@mv -f lib/mmck.lang.lists.temp $@

lib/mmck.lang.lists.import.scm lib/mmck.lang.lists.$(OBJEXT): lib/mmck.lang.lists.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.lang.lists.lock lib/mmck.lang.lists.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.lang.lists.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.lang.lists.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.lang.lists.stamp; \
	    $$dry rmdir lib/mmck.lang.lists.lock; \
	  else \
	    while test -d lib/mmck.lang.lists.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.lang.lists.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.lang.lists.import.$(MMCK_SOEXT): lib/mmck.lang.lists.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck lang): module vectors

MMCK_LANG_MODULES		+= lib/mmck.lang.vectors.scm
MMCK_LANG_RUNTIME_OBJECTS	+= lib/mmck.lang.vectors.$(OBJEXT)
MMCK_LANG_IMPORT_SOURCES	+= lib/mmck.lang.vectors.import.scm
MMCK_LANG_IMPORT_DEPS		+= lib/mmck.lang.vectors.import.$(MMCK_SOEXT)
MMCK_LANG_IMPORT_SHARED_LIBS	+= lib/mmck.lang.vectors.import.$(MMCK_SOEXT)
MMCK_LANG_TEMP_FILES		+= \
	lib/mmck.lang.vectors.temp	\
	lib/mmck.lang.vectors.lock	\
	lib/mmck.lang.vectors.stamp

MMCK_LANG_VECTORS_DEPS = \
	lib/mmck.lang.debug.import.$(MMCK_SOEXT)	\
	lib/mmck.lang.core.import.$(MMCK_SOEXT)		\
	lib/mmck.lang.lists.import.$(MMCK_SOEXT)	\
	$(MMCK_OBJECTS_DEPS)

lib/mmck.lang.vectors.stamp: lib/mmck.lang.vectors.scm $(MMCK_LANG_VECTORS_DEPS)
	@rm -f lib/mmck.lang.vectors.temp
	@touch lib/mmck.lang.vectors.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.lang.vectors.$(OBJEXT) $(<)
	@$(MV) mmck.lang.vectors.import.scm lib/mmck.lang.vectors.import.scm
	@mv -f lib/mmck.lang.vectors.temp $@

lib/mmck.lang.vectors.import.scm lib/mmck.lang.vectors.$(OBJEXT): lib/mmck.lang.vectors.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.lang.vectors.lock lib/mmck.lang.vectors.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.lang.vectors.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.lang.vectors.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.lang.vectors.stamp; \
	    $$dry rmdir lib/mmck.lang.vectors.lock; \
	  else \
	    while test -d lib/mmck.lang.vectors.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.lang.vectors.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.lang.vectors.import.$(MMCK_SOEXT): lib/mmck.lang.vectors.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck lang): module strings

MMCK_LANG_MODULES		+= lib/mmck.lang.strings.scm
MMCK_LANG_RUNTIME_OBJECTS	+= lib/mmck.lang.strings.$(OBJEXT)
MMCK_LANG_IMPORT_SOURCES	+= lib/mmck.lang.strings.import.scm
MMCK_LANG_IMPORT_DEPS		+= lib/mmck.lang.strings.import.$(MMCK_SOEXT)
MMCK_LANG_IMPORT_SHARED_LIBS	+= lib/mmck.lang.strings.import.$(MMCK_SOEXT)
MMCK_LANG_TEMP_FILES		+= \
	lib/mmck.lang.strings.temp	\
	lib/mmck.lang.strings.lock	\
	lib/mmck.lang.strings.stamp

MMCK_LANG_STRINGS_DEPS = \
	lib/mmck.lang.debug.import.$(MMCK_SOEXT)	\
	lib/mmck.lang.core.import.$(MMCK_SOEXT)		\
	$(MMCK_OBJECTS_DEPS)

lib/mmck.lang.strings.stamp: lib/mmck.lang.strings.scm $(MMCK_LANG_STRINGS_DEPS)
	@rm -f lib/mmck.lang.strings.temp
	@touch lib/mmck.lang.strings.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.lang.strings.$(OBJEXT) $(<)
	@$(MV) mmck.lang.strings.import.scm lib/mmck.lang.strings.import.scm
	@mv -f lib/mmck.lang.strings.temp $@

lib/mmck.lang.strings.import.scm lib/mmck.lang.strings.$(OBJEXT): lib/mmck.lang.strings.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.lang.strings.lock lib/mmck.lang.strings.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.lang.strings.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.lang.strings.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.lang.strings.stamp; \
	    $$dry rmdir lib/mmck.lang.strings.lock; \
	  else \
	    while test -d lib/mmck.lang.strings.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.lang.strings.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.lang.strings.import.$(MMCK_SOEXT): lib/mmck.lang.strings.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck lang): main module

MMCK_LANG_MODULES		+= lib/mmck.lang.scm
MMCK_LANG_RUNTIME_OBJECTS	+= lib/mmck.lang.$(OBJEXT)
MMCK_LANG_IMPORT_SOURCES	+= lib/mmck.lang.import.scm
MMCK_LANG_IMPORT_SHARED_LIBS	+= lib/mmck.lang.import.$(MMCK_SOEXT)
MMCK_LANG_TEMP_FILES		+= \
	lib/mmck.lang.temp				\
	lib/mmck.lang.lock				\
	lib/mmck.lang.stamp

# This is the main compilation unit and it defines the main module; it
# depends upon all the import libraries of the other modules.
#
lib/mmck.lang.stamp: lib/mmck.lang.scm $(MMCK_LANG_IMPORT_DEPS) $(MMCK_OBJECTS_DEPS)
	@rm -f lib/mmck.lang.temp
	@touch lib/mmck.lang.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.lang.$(OBJEXT) $(<)
	@$(MV) mmck.lang.import.scm lib/mmck.lang.import.scm
	@mv -f lib/mmck.lang.temp $@

lib/mmck.lang.import.scm lib/mmck.lang.$(OBJEXT): lib/mmck.lang.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.lang.lock lib/mmck.lang.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.lang.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.lang.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.lang.stamp; \
	    $$dry rmdir lib/mmck.lang.lock; \
	  else \
	    while test -d lib/mmck.lang.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.lang.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.lang.import.$(MMCK_SOEXT): lib/mmck.lang.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck lang): wrapping up

EXTRA_DIST	+= $(MMCK_LANG_MODULES)

CLEANFILES	+= \
	$(MMCK_LANG_RUNTIME_SHARED_LIBS)	\
	$(MMCK_LANG_RUNTIME_OBJECTS)		\
	$(MMCK_LANG_IMPORT_SHARED_LIBS)		\
	$(MMCK_LANG_IMPORT_SOURCES)		\
	$(MMCK_LANG_TEMP_FILES)

lib/mmck.lang.$(MMCK_SOEXT): $(MMCK_LANG_RUNTIME_OBJECTS)
	$(MMCK_LIB_ENV) $(CSC_LINK_LIBRARY) $(@) $(^)


#### shared libraries installation

# All the import shared libraries that we want to installe.
#
MMCK_ALL_IMPORT_SHARED_LIBS	= \
	$(MMCK_LANG_IMPORT_SHARED_LIBS)

# All the runtime shared libraries that we want to install.
#
MMCK_ALL_RUNTIME_SHARED_LIBS	= \
	$(MMCK_LANG_RUNTIME_SHARED_LIBS)

# These variables are parsed by GNU Automake to build the main library
# targets and install them under the directory $(MMUX_CHICKEN_LIBDIR).
#
# We are shamelessly cheating by  installing them using GNU Automake's
# primary  suffix  "_SCRIPTS"; these  are  not  scripts!  Scripts  are
# installed with the permissions 0755, which, for some reason, is what
# we want  for shared libraries.
#
# We  could use  the  primary suffix  "_DATA", but  we  would get  the
# permissions 0644.
#
sharedlibdir		= $(MMUX_CHICKEN_LIBDIR)
sharedlib_SCRIPTS	= $(MMCK_ALL_RUNTIME_SHARED_LIBS) $(MMCK_ALL_IMPORT_SHARED_LIBS)


#### interface to "make check"
#
# Read "Parallel Test Harness" in the documentation of GNU Automake to
# understand how to use this interface for "make check".
#

# The list of object files.  We will store it in CLEANFILES.
#
MMCK_CHECK_OBJECTS	=

# The list of test programs.  We will set TESTS to it.
#
MMCK_CHECK_PROGS	=

# Dependencies common to all the test programs.
#
MMCK_CHECK_DEPS		+= $(MMCK_ALL_RUNTIME_SHARED_LIBS) $(MMCK_ALL_IMPORT_LIBS)

## --------------------------------------------------------------------

EXTRA_DIST		+= tests/test-version.scm
MMCK_CHECK_OBJECTS	+= tests/test-version.$(OBJEXT)
MMCK_CHECK_PROGS	+= tests/test-version.exe

tests/test-version.$(OBJEXT): tests/test-version.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-version.exe: tests/test-version.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

EXTRA_DIST		+= tests/test-lang-core.scm
MMCK_CHECK_OBJECTS	+= tests/test-lang-core.$(OBJEXT)
MMCK_CHECK_PROGS	+= tests/test-lang-core.exe

tests/test-lang-core.$(OBJEXT): tests/test-lang-core.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-lang-core.exe: tests/test-lang-core.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

EXTRA_DIST		+= tests/test-lang-booleans.scm
MMCK_CHECK_OBJECTS	+= tests/test-lang-booleans.$(OBJEXT)
MMCK_CHECK_PROGS	+= tests/test-lang-booleans.exe

tests/test-lang-booleans.$(OBJEXT): tests/test-lang-booleans.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-lang-booleans.exe: tests/test-lang-booleans.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

EXTRA_DIST		+= tests/test-lang-lists.scm
MMCK_CHECK_OBJECTS	+= tests/test-lang-lists.$(OBJEXT)
MMCK_CHECK_PROGS	+= tests/test-lang-lists.exe

tests/test-lang-lists.$(OBJEXT): tests/test-lang-lists.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-lang-lists.exe: tests/test-lang-lists.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

EXTRA_DIST		+= tests/test-lang-vectors.scm
MMCK_CHECK_OBJECTS	+= tests/test-lang-vectors.$(OBJEXT)
MMCK_CHECK_PROGS	+= tests/test-lang-vectors.exe

tests/test-lang-vectors.$(OBJEXT): tests/test-lang-vectors.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-lang-vectors.exe: tests/test-lang-vectors.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

EXTRA_DIST		+= tests/test-lang-strings.scm
MMCK_CHECK_OBJECTS	+= tests/test-lang-strings.$(OBJEXT)
MMCK_CHECK_PROGS	+= tests/test-lang-strings.exe

tests/test-lang-strings.$(OBJEXT): tests/test-lang-strings.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-lang-strings.exe: tests/test-lang-strings.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)


#### wrapping up tests

CLEANFILES	+= $(MMCK_CHECK_PROGS) $(MMCK_CHECK_OBJECTS)

if MMCK_TESTING_ENABLED
TESTS		= $(MMCK_CHECK_PROGS)
endif

### end of file
