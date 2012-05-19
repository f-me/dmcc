#!/bin/sh

AVAYA_AES_DIR=`pwd`
DEPS_DIR=$AVAYA_AES_DIR/deps
REPOS=("git@github.com:/Elemir/HaXml.git")
APPL_SESSION_XSD=("stop-application-session.xsd" 
                  "start-application-session.xsd"
                  "reset-application-session-timer.xsd")
[ $# -eq 0 ] && echo "usage: $0 <cmapixml-sdk path>" 2>/dev/null && exit 1

if [[ ! -d "cmapixml-sdk" ]]; then
  tar -xjf $1
  cp cmapixml-sdk/xsd/avaya-csta-schemas/* xsd/
  cp cmapixml-sdk/xsd/csta-schemas/* xsd/
fi

pushd xsd
for xsd in "${APPL_SESSION_XSD[@]}"; do
    rm -f $xsd
    wget "http://www.ecma-international.org/standards/ecma-354/appl_session/$xsd"
done
popd

FRESH_BUILD=0
if [[ ! -d "$DEPS_DIR" ]]; then
  mkdir "$DEPS_DIR"
  FRESH_BUILD=1
fi

cd "$DEPS_DIR"
repos_to_rebuild=("$AVAYA_AES_DIR/tools/xsd2hs")

for repo in "${REPOS[@]}" ; do
  rPath=${repo##*/}
  rPath="$DEPS_DIR/${rPath%*.git}"
  if [[ ! -d "$rPath" ]]; then
    echo "Cloning dependent repo: $repo to $rPath"
    git clone $repo
    repos_to_rebuild=("${repos_to_rebuild[@]}" "$rPath")
  fi
done


function CheckRepo {
  repo_path=$1
  cd $repo_path
  echo "Checking $repo_path"

  git status -uno | grep "modified" > /dev/null
  if [[ $? -eq o ]]; then
    echo "    .. modified locally"
    repos_to_rebuild=("${repos_to_rebuild[@]}" "`pwd`")
    return
  fi

  git remote -v update 2>&1 | grep "up to date.*master" > /dev/null
  if [[ $? -eq 0 ]]; then
    echo "    .. up-to date"
    return
  fi

  git status -uno | grep "can be fast-forwarded" > /dev/null
  if [[ $? -eq 0 ]]; then
    echo "    .. pulling changes from remote"
    if git pull; then
      echo "    .. successfully updated"
      repos_to_rebuild=("${repos_to_rebuild[@]}" "`pwd`")
      return
    fi
  fi

  echo "    .. something went wrong"
  exit 1
}

for repo in "${REPOS[@]}"; do
  rPath=${repo##*/}
  rPath="$DEPS_DIR/${rPath%*.git}"
  CheckRepo $rPath
done

for repo in "${REPOS[@]}"; do
  rPath=${repo##*/}
  rPath="$DEPS_DIR/${rPath%*.git}"
  CheckRepo $rPath
done

cd "$AVAYA_AES_DIR"

for repo in "${repos_to_rebuild[@]}"; do
  cabal-dev add-source $repo
done

for repo in "${repos_to_rebuild[@]}"; do
  package_name=${repo##*/}
  cabal-dev install --force-reinstalls --reinstall $package_name
  if [[ ! $? -eq 0 ]]; then
    exit 1
  fi
done

if [[ $FRESH_BUILD -eq 1 ]]; then
  cabal-dev install-deps
  if [[ ! $? -eq 0 ]]; then
    exit 1
  fi
fi

./cabal-dev/bin/xsd2hs xsd/start-application-session.xsd xsd/stop-application-session.xsd xsd/reset-application-session-timer.xsd xsd/avaya-csta.xsd 
cabal-dev configure
cabal-dev build
cabal-dev install

