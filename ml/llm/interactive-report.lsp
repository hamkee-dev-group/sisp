; Keep machine startup diagnostics separate from the completion shown to the
; human.  The launcher reads this ignored temporary output file.
(write-file *llm-interactive-output-path* *llm-interactive-completion*)

