set -x EDITOR nvim
set -x VISUAL nvim

fish_add_path -p -P /home/lorenz/.local/bin   # add local bin
fish_add_path -p -P /home/lorenz/dev/apps/zig # add zig
fish_add_path -p -P /opt/rocm-5.4.0/bin       # add hipcc to the path (for gpu rendering in blender)

if test -z (pgrep ssh-agent | string collect)
	eval (ssh-agent -c)
	set -Ux SSH_AUTH_SOCK $SSH_AUTH_SOCK
	set -Ux SSH_AGENT_PID $SSH_AGENT_PID
end

if status is-interactive
	# Commands to run in interactive sessions can go here
	set fish_greeting
	starship init fish | source
	alias vi=nvim
	alias vim=nvim
end
