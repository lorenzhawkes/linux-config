# Defined in /home/lorenz/.config/fish/functions/to_mp4.fish @ line 1
function to_mp4
HandBrakeCLI -i $argv[1] --main-feature -o $argv[2].mp4 -f av_mp4 --all-audio --all-subtitles --subtitle-burned=none --subtitle-default=none
end
