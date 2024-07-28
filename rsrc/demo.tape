# NOTE: This tape file uses a new feature (KeyStrokes) that is under review from
# vhs. Until my PR is merged, this tape file is used with a build of vhs from my
# feature branch.
# See the PR: https://github.com/charmbracelet/vhs/pull/496
Output rsrc/demo.mp4
Output rsrc/demo.gif

Require jam
Require gum

Set Shell "bash"
Set FontSize 24
Set Width 1200
Set Height 600
Set KeyStrokes Show

Type "jam"
Sleep 500ms
Enter
Sleep 2s
Type "b"
Sleep 1s
Type "d"
Sleep 1s
Type "?"
Sleep 3s
Backspace
Sleep 1s
Type "?"
Sleep 1s
Type "."
Sleep 4500ms 
Type "jam"
Sleep 300ms 
Enter
Sleep 1500ms 
Type "r"
Sleep 2s
