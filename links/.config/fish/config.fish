function fish_prompt --description 'Write out the prompt'

  set -l last_status $status

  set_color $fish_color_cwd
  echo -n (prompt_pwd)
  set_color normal
  set __fish_git_prompt_showdirtystate 1
  set __fish_git_prompt_showstashstate 1
  set __fish_git_prompt_showupstream 'auto'
  set __fish_git_prompt_show_informative_status 1

  __fish_git_prompt

  if not test $last_status -eq 0
    set_color $fish_color_error
  end

  echo -n ' $ '

end

fish_vi_key_bindings

set fish_function_path $fish_function_path "/usr/share/powerline/bindings/fish"
source /usr/share/powerline/bindings/fish/powerline-setup.fish
powerline-setup
