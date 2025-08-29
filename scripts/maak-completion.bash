maak_file_name="maak.scm"

maak_completions() {
  if [[ ! -f "${maak_file_name}" ]]; then
    return
  fi

  local current_word="${COMP_WORDS[COMP_CWORD]}"

  local opts
  opts=$(awk '/^\(define \(/ { gsub("[()]", ""); print $2 }' "${maak_file_name}")

  COMPREPLY=($(compgen -W "${opts}" -- "${current_word}"))
  return 0
}

complete -F maak_completions maak
