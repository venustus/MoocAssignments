#!/bin/bash

# Log all of our output too...
if [ "$INSTALL_RUBY_WITH_TK_SCRIPT_LOGGING" != "$0" ]
then
    export INSTALL_RUBY_WITH_TK_SCRIPT_LOGGING="$0" 
    bash "$0" $* | tee -a "$0.log"
    exit $?
fi

DEFAULT_DIR_NAME='ruby1.9.3-with-tk'
SCRIPT_NAME="$DEFAULT_DIR_NAME"

DEFAULT_BASE="$HOME/$DEFAULT_DIR_NAME"

# If a first argument is given, it is a path where we will create a directory
# and install ruby, etc.
BASE="$1"
case "$BASE" in
    /*) ;;
    *) test -z "$BASE" || BASE="$PWD/$BASE"
esac
# If not specified, we default to the directory hardcoded above.
test -z "$BASE" && BASE="$DEFAULT_BASE"

# Tools/libraries that should already exist.
GCC=/opt/gcc-4.8.1/bin/gcc
TCL_TK_VERSION=8.5
SYSTEM=/System/Library/Frameworks
LIBRARY=/Library/Frameworks
SYSTEM_TCL=$SYSTEM/Tcl.framework/Versions/$TCL_TK_VERSION
SYSTEM_TK=$SYSTEM/Tk.framework/Versions/$TCL_TK_VERSION
LIBRARY_TCL=$LIBRARY/Tcl.framework/Versions/$TCL_TK_VERSION
LIBRARY_TK=$LIBRARY/Tk.framework/Versions/$TCL_TK_VERSION

# Stuff we will create.
BREW_BASE="$BASE/homebrew"
BREW_BIN="$BREW_BASE/bin"
BREW="$BREW_BIN/brew"
BIN_LINKS="$BASE/bin"
RUBY="$BREW_BIN/ruby"
IRB="$BREW_BIN/irb"
GEM="$BREW_BIN/gem"

# Links to add selected items in $BREW_BIN to the $PATH.
RUBY_LINK="$BIN_LINKS/ruby"
IRB_LINK="$BIN_LINKS/irb"
GEM_LINK="$BIN_LINKS/gem"

# Scrub the environment:
unset GEM_PATH
unset GEM_HOME
unset HOMEBREW_CELLAR
unset HOMEBREW_LIBRARY_PATH
unset HOMEBREW_PREFIX
unset HOMEBREW_REPOSITORY

# By default brew tries to use /Library/Cachs/Homebrew regardless of where it is
# installed.
export HOMEBREW_CACHE="$BASE/homebrew-cache"

# Marks how far we've made it in the install process.
CURSOR="$BASE/.ruby_install_next_stage"

LINESEP="======================================================================"

# Prefixed echo.
function log() {
    echo "[$SCRIPT_NAME]  $*"
}
function logn() {
    echo -n "[$SCRIPT_NAME]  $*"
}

# Fail and save the cursor.
function fail() {
    log "$STAGE: $MESSAGE  [FAILED]"
    test -e "$BASE" && echo "NEXT_STAGE=$STAGE" > "$CURSOR"
    log "Failure happened with environment:"
    env
    log "$STAGE: $MESSAGE  [FAILED]"
    exit $STAGE
}

# Filter out work that's already been done.
function stagen() {
    STAGE=$(($STAGE+1))
    if [ "$NEXT_STAGE" -gt "$STAGE" ]
    then
        log "$STAGE. $1  [done]"
        return 0
    else
        test -z "$2" || (test -e "$BASE" && echo "NEXT_STAGE=$STAGE" > "$CURSOR")
        test -z "$MESSAGE" || log "$(($STAGE-1)). $MESSAGE  [done]"
        MESSAGE="$1"
        echo ""
        echo "$LINESEP"
        echo ""
        log "$STAGE. $1  [start]"
        return 1
    fi
}
function stage() {
    stagen "$1" record
}

function check_macosx() {
    uname -a
    test "Darwin" = "`uname`" || return -1
    case `uname -r` in
        10.*) # Mac OS X 10.6 Snow Leopard
            ;;
        11.*) # Mac OS X 10.7 Lion
            ;;
        12.*) # Mac OS X 10.8 Mountain Lion
            ;;
        13.*) # Mac OS X 10.9 Mavericks
            EXTRA_TK_GEM_FLAGS="--with-X11-include=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.9.sdk/System/Library/Frameworks/Tk.framework/Versions/8.5/Headers"
            ;;
        *) # unknown
            return -2
            ;;
    esac
    return 0
}



# recover anything done so far.
MESSAGE=""
STAGE=0
NEXT_STAGE=0
test -e "$CURSOR" && . "$CURSOR"

log "Starting `date`"
cat <<EOF

$LINESEP

This script for Mac OS X tries to install ruby 1.9.3 with tk$TCL_TK_VERSION:
  * The installation (including homebrew) will be entirely self-contained
    and will not affect any tools you have installed already.
  * It will modify .bash_profile if you ask it to.
  * It will install in $BASE and create a log in $0.log
  * Renaming or moving $BASE might break ruby.
  * To uninstall, delete $BASE
  * To install elsewhere, run:
      bash $0 "elsewhere"
EOF

if [ "$NEXT_STAGE" -gt "1" ]
then
    echo
    echo "$LINESEP"
    echo
fi

# Check Mac OS X.
stage "Check Mac OS X." || (check_macosx && logn "This script should work on this version of Mac OS X.  Hit RETURN to continue or Control-C to cancel." && read) || (logn "This is not a Mac OS X I know about.  Hit Control-C to cancel or RETURN to continue." && read && log "Attempting install on unknown Mac OS X.")

# Check for dependencies.
stage "Check for Xcode commmand line tools." || (test -x "$GCC" && "$GCC" --version) || (log "Please install Xcode and its command line tools." && open "https://developer.apple.com/xcode/" && exit $STAGE) || fail

stage "Check for Tcl/Tk." || (test -z "$FORCE_SYSTEM_TK" && test -e "$LIBRARY_TCL" && test -e "$LIBRARY_TK/Headers" && echo "WARNING: found $LIBRARY_TK" && echo "I will try to use it instead of $SYSTEM_TK."  && echo "If that doesn't work, try running the following variation on this script:" && echo && echo "  env FORCE_SYSTEM_TK=yes bash $0" && echo && echo -n "Hit RETURN to continue." && read) || (test -e "$SYSTEM_TCL" && test -e "$SYSTEM_TK/Headers" && echo "Found $SYSTEM_TK") || (echo "Could not find a full Tcl/Tk $TCL_TK_VERSION.  Please install ActiveTcl 8.5 (NOT 8.6) from http://www.activestate.com/activetcl/downloads and run this script again." && exit $STAGE) || fail

# Try to create the directory.  If it exists, refuse to do anything with it.
stage "Create $BASE." || (mkdir "$BASE" && mkdir "$BREW_BASE" && mkdir "$BIN_LINKS" && ls -lh "$BASE") || fail

# Get homebrew.
stage "Download homebrew to $BREW_BASE." || (curl -L https://github.com/mxcl/homebrew/tarball/master | tar xz --strip 1 -C "$BREW_BASE") || fail
stage "Check homebrew." || "$BREW" --version || fail

# Install ruby.
stage "Tap alternate ruby versions." || "$BREW" tap homebrew/versions || fail
stage "Install ruby.  This will take a while." || "$BREW" install ruby193 || fail

# Check ruby.
stage "Check ruby." || ("$RUBY" --version) || fail

# Install ruby/tk bindings.
if test -z "$FORCE_SYSTEM_TK" && test -e "$LIBRARY_TK"
then
    TK="$LIBRARY_TK"
    FRAMEWORKS="$LIBRARY"
else
    TK="$SYSTEM_TK"
    FRAMEWORKS="$SYSTEM"
fi
TK_GEM_FLAGS="--without-X11 --with-X11-include=$TK/Headers --with-tcltk-version=$TCL_TK_VERSION $EXTRA_TK_GEM_FLAGS" # --with-tcltk-framework=$FRAMEWORKS"
stage "Install ruby/tk bindings for $TK.  This could take a while." || "$GEM" install tk_as_gem -- "$TK_GEM_FLAGS" || (log "Failed to build with tk.  Please try installing ActiveTcl 8.5 (NOT 8.6) from http://www.activestate.com/activetcl/downloads and run this script again." && exit $STAGE) || fail

# Test ruby/tk.
stage "Check ruby/tk." || ("$RUBY" -e "require 'tk'; puts 'Success.'") || fail

# Configure environment.
stage "Create symlinks in $BIN_LINKS." || (rm -f "$BIN_LINKS"/* && ln -s "$RUBY" "$RUBY_LINK" && ln -s "$IRB" "$IRB_LINK" && ln -s "$GEM" "$GEM_LINK" && ls -lh "$BIN_LINKS") || fail

function setup_dotfiles() {
    # Create -tk aliases.
    echo "alias ruby-tk='$RUBY'" > "$BASE/aliases"
    echo "alias irb-tk='$IRB'" >> "$BASE/aliases"
    echo "alias gem-tk='$GEM'" >> "$BASE/aliases"

    # Create path setup.
    echo "export PATH='$BIN_LINKS'":'$PATH' > "$BASE/paths"

    # Offer to put this ruby/irb/gem on the PATH.
    echo
    echo "Do you want to use this version of ruby by default?"
    echo "(The command 'ruby' would run this version of ruby.)"
    echo -n "Type 'yes' or 'no', then RETURN [default = no]:  "
    read ANSWER
    echo
    ALSO=""
    case "$ANSWER" in
        yes*|Yes*|YES*)
            echo >> "$HOME/.bash_profile"
            echo >> "$HOME/.bash_profile"
            echo "# PATH additions for $BASE" >> "$HOME/.bash_profile"
            echo "test -e $BASE/paths && source $BASE/paths" >> "$HOME/.bash_profile"

            echo "Edited $HOME/.bash_profile to add ruby/irb/gem to the PATH for future shells."
            echo "Run 'source $BASE/paths' to get this in the current shell."
            ALSO="also "
            ;;
        *)
            echo "Leaving ruby/irb/gem alone.  If you change your mind later, rerun this script."
            echo "                             It will skip straight to this step."
            ;;
    esac

    # Offer to create ruby-tk/irb-tk/gem-tk aliases.
    echo
    echo "Do you ${ALSO}want this version of ruby aliased as 'ruby-tk'?"
    echo "(The command 'ruby-tk' would run this version of ruby.)"
    echo -n "Type 'yes' or 'no', then RETURN [default = no]:  "
    read ANSWER
    echo
    case "$ANSWER" in
        yes*|Yes*|YES*)
            echo >> "$HOME/.bash_profile"
            echo >> "$HOME/.bash_profile"
            echo "# Aliases for $BASE" >> "$HOME/.bash_profile"
            echo "test -e $BASE/aliases && source $BASE/aliases" >> "$HOME/.bash_profile"

            echo "Edited $HOME/.bash_profile to add ruby-tk/irb-tk/gem-tk aliases in future shells."
            echo "Run 'source $BASE/aliases' to get this in the current shell."
            ;;
        *)
            echo "Leaving aliases alone.  If you change your mind later, rerun this script."
            echo "                        It will skip straight to this step."
            ;;
    esac
    echo
}

stage "Configure environment." || setup_dotfiles

stagen "Start using ruby with tk!"
# rm "$CURSOR"
