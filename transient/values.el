((gptel-menu "g*Jarvis*")
 (magit-log:magit-log-mode "-n256" "--graph" "--color" "--decorate" "--show-signature")
 (magit-pull "--ff-only")
 (rg-menu "--hidden"))
