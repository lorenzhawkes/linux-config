function git-logall
git log --tags --branches -300 --graph --decorate=full --name-status $argv;
end
