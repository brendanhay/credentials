_credentials()
{
    local cmdline
    CMDLINE=(--bash-completion-index $COMP_CWORD)

    for arg in ${COMP_WORDS[@]}; do
        CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)
    done

    COMPREPLY=( $(credentials "${CMDLINE[@]}") )
}

complete -o filenames -F _credentials credentials
