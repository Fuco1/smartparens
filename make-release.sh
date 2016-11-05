#!/bin/bash -e

if [ -z $1 ]; then
    echo "Usage: ./make-release.sh <version>"
    exit 1
fi

VERSION=$1

if git tag | grep -q "$VERSION" ; then
    echo "Version $VERSION already exists; use \`git tag -d $VERSION' to remove the tag"
    exit 1
fi

echo -n "Updating Cask... "
SEDSCRIPT='s/(package .*?)("[0-9.]+")/\1'"\"${VERSION}\""'/'
sed -i -re "$SEDSCRIPT" Cask
echo "done"

echo -n "Updating pkg file... "
cask pkg-file
echo "done"

echo -n "Create release commit... "
git add Cask
git add smartparens-pkg.el
git commit -m "Release $VERSION"
echo "done"

echo -n "Updating git tags... "
git tag -f "$VERSION"
echo "done"
