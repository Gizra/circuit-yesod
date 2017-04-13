# @todo: Manipulate config
stack build
TARGET=deploy/dist
mkdir "$TARGET"
cp `find -executable -type f | grep circuit-yesod$ | head -n1` "$TARGET"
cp -r static $TARGET
cp -r config $TARGET
