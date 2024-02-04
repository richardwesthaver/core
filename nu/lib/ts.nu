# ts.nu
export def "ts-lang install" [lang:string] {

}

declare -ar default_langs=(
  commonlisp bash c cpp css go html java javascript jsdoc json python regex rust
  typescript/tsx typescript/typescript yaml
) 
# see https://tree-sitter.github.io/tree-sitter/#parsers for a
# complete list of parsers available
TARGETDIR="${1:-build/src/tree-sitter-langs}"
PREFIX=${PREFIX:-/usr/local}
CC=${CC:-clang}
CXX=${CXX:-clang++}
if [ $(uname) == "Darwin" ];then
   EXT=dylib;
else
   EXT=so
fi

declare -A repos
repos[commonlisp]=https://github.com/theHamsta/tree-sitter-commonlisp.git
repos[yaml]=https://github.com/ikatyang/tree-sitter-yaml.git
repos[cpp]=https://github.com/ruricolist/tree-sitter-cpp.git

declare -a langs
if [ -z "${2:-}" ]; then
  langs=(${default_langs[@]})
else
  langs=($@)
fi

mkdir -pv $TARGETDIR
pushd $TARGETDIR
for lang in "${langs[@]}";do
  [ -d "tree-sitter-${lang%/*}" ] || git clone ${repos[$lang]:-https://github.com/tree-sitter/tree-sitter-${lang%/*}};
  # subshell
  (
    cd "tree-sitter-${lang}/src";
    if test -f "scanner.cc"; then
      ${CXX} -I. -fPIC scanner.cc -c -lstdc++;
      ${CC} -I. -std=c99 -fPIC parser.c -c;
      ${CXX} -shared scanner.o parser.o -o ${PREFIX}/lib/libtree-sitter-"${lang//\//-}.${EXT}";
    elif test -f "scanner.c"; then
      ${CC} -I. -std=c99 -fPIC scanner.c -c;
      ${CC} -I. -std=c99 -fPIC parser.c -c;
      ${CC} -shared scanner.o parser.o -o ${PREFIX}/lib/libtree-sitter-"${lang//\//-}.${EXT}";
    else
      ${CC} -I. -std=c99 -fPIC parser.c -c;
      ${CC} -shared parser.o -o ${PREFIX}/lib/libtree-sitter-"${lang//\//-}.${EXT}";
    fi;
    mkdir -p "${PREFIX}/share/tree-sitter/${lang}/";
    cp grammar.json node-types.json "${PREFIX}/share/tree-sitter/${lang}";
  )
done
