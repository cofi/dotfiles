# -*- mode: sh -*-

export ZSHDIR=$HOME/config/dotfiles/zsh
export LOCALDIR=$HOME/.zsh_local
export PATH=$PATH:$HOME/.bin

fpath=($ZSHDIR/zsh-completions/src $fpath)

function load_config() {
    if [[ -f $1 ]] {
        source $1
    }
}

load_config $LOCALDIR/pre

# load extra modules
autoload -U zmv
autoload -Uz vcs_info
autoload run-help

zmodload -i zsh/complist

### OPTIONS ###
setopt correct no_correct_all
#Do hashing
setopt hash_cmds hash_dirs hash_list_all
unsetopt print_exit_value
#dont wait or ask when `rm *`
setopt no_rm_star_wait rm_star_silent
setopt short_loops

### expansion+globbing options
setopt brace_ccl brace_expand
setopt equals
setopt rc_expand_param
unsetopt magic_equal_subst mark_dirs nomatch sh_glob

### history options
setopt append_history
setopt hist_allow_clobber            ### add '|' to output redirections in the history.
setopt no_hist_beep

#keep historyfile clean
setopt hist_ignore_dups hist_ignore_all_dups hist_save_no_dups hist_ignore_space hist_no_store
setopt hist_verify
setopt hist_reduce_blanks inc_append_history share_history

### completion options
setopt auto_param_keys          ### intelligently remove automatically inserted characters

#add slash when completing dir but remove intelligently
setopt auto_param_slash auto_remove_slash

setopt complete_aliases complete_in_word
setopt list_ambiguous
setopt list_types
unset list_beep

### script+function options
setopt        function_arg_zero
setopt        local_options
setopt        multios
setopt     no_verbose
setopt     no_xtrace
setopt        local_traps
setopt        local_options

### prompt options
setopt prompt_subst
setopt prompt_percent

setopt emacs
setopt interactive_comments

### cd options
setopt auto_pushd pushd_ignore_dups
setopt chase_dots chase_links
setopt auto_cd
export PROMPT="%F{green}%n@%m %~ %#> %F{white}"

if [[ -d $ZSHDIR ]] {
    load_config $ZSHDIR/colors
    load_config $ZSHDIR/dirhash
    load_config $ZSHDIR/alias
    load_config $ZSHDIR/functions/common
    load_config $ZSHDIR/auto_rehash
    load_config $ZSHDIR/abbrev_expansion
    load_config $ZSHDIR/completion
    load_config $ZSHDIR/extensions
    load_config $ZSHDIR/bindkey
    case "$TERM" in
        linux)
            ;;
        dumb)
            export PROMPT="%F{green}%n@%m %d %#> %F{white}"
            ;;
        *) load_config $ZSHDIR/prompt
            ;;
    esac
    load_config $ZSHDIR/style
}

load_config $LOCALDIR/post

load_config $ZSHDIR/zsh-substring-search/zsh-history-substring-search.zsh

unfunction load_config

HISTFILE=~/.zshhistory
HISTSIZE=5000
SAVEHIST=8000

export EDITOR=vim
export PAGER=less

export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# correct prompt
export SPROMPT="Correct $fg[red]%R$reset_color to $fg[green]%r$reset_color? (%Uy%ues, %Un%uo, %Ua%ubort, %Ue%udit) "

if [[ -z "$SSH_CONNECTION" ]] {
	source ~/.keychain/$HOST-sh
    source ~/.keychain/$HOST-sh-gpg
}

# only allow user to read and write by default
umask 077
