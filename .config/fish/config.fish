set -x EDITOR vim

# add local bin
# add hipcc to the path (for gpu rendering in blender)
set -x PATH /home/lorenz/.local/bin /home/lorenz/dev/apps/zig /opt/rocm-5.4.0/bin $PATH

if test -z (pgrep ssh-agent | string collect)
	eval (ssh-agent -c)
	set -Ux SSH_AUTH_SOCK $SSH_AUTH_SOCK
	set -Ux SSH_AGENT_PID $SSH_AGENT_PID
end

starship init fish | source

alias vi=vim
