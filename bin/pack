#!/bin/zsh
emulate -L zsh
ename=$0
usage () {
    cat << EOF
Usage: $ename [-f format] [-o output] file,[file,[...]]
  -h     Show this message.
  -f     Specify format. Default to xz
         Legal formats: gz, bz2, lzma, xz, zip
  -o     Output file (format is appended).
EOF
}

format=
output=

while getopts "hf:o:" option; do
    case "$option" in
        h)
            usage
            exit 1 ;;
        f)
            format="$OPTARG" ;;
        o)
            output="$OPTARG" ;;
    esac
done
shift $((OPTIND-1))

# default to xz format
format=${format:-xz}

# Use first file as output if none is specified
output=${output:-$1}

# check for missing files
if [[ ${#@} -eq 0 ]]; then
    usage
    exit 1
fi

complete=
# tar files if necessary
if [[ ${#@} -gt 1 || -d "$@" ]]; then
    case "$format" in
        zip) ;;
        gz)
            tar -acf "${output}.tar.gz" "$@"
            complete=1;;
        bzip2)
            tar -acf "${output}.tar.bzip2" "$@"
            complete=1;;
        lzma)
            tar -acf "${output}.tar.lzma" "$@"
            complete=1;;
        xz)
            tar -acf "${output}.tar.xz" "$@"
            complete=1;;
        *)   
            output="${output}.tar"
            tar -cf "$output" "$@"
            ;;
    esac
fi

if [[ -n "$complete" ]]; then; exit 0; fi

case "$format" in
    gz)
        gzip -rc "$@" > "${output}.gz";;
    bzip2)
        bzip2 -kz "$@" ;;
    lzma)
        lzma -zk "$@" ;;
    xz)
        xz -zk "$@" ;;
    zip)
        zip -r "${output}" "$@" ;;
esac
