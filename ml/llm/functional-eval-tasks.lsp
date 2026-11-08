; Frozen prompts and continuations are outside every corpus input.
(functional-eval-one "list-sum"
  "ml/llm/eval/list-sum.prompt" "ml/llm/eval/list-sum.target")
(functional-eval-one "list-reverse"
  "ml/llm/eval/list-reverse.prompt" "ml/llm/eval/list-reverse.target")
(functional-eval-one "string-count"
  "ml/llm/eval/string-count.prompt" "ml/llm/eval/string-count.target")
(functional-eval-one "set-common"
  "ml/llm/eval/set-common.prompt" "ml/llm/eval/set-common.target")
(functional-eval-one "matrix-trace"
  "ml/llm/eval/matrix-trace.prompt" "ml/llm/eval/matrix-trace.target")
(functional-eval-one "binary-search"
  "ml/llm/eval/binary-search.prompt" "ml/llm/eval/binary-search.target")
(functional-eval-one "comment-to-code"
  "ml/llm/eval/comment-to-code.prompt" "ml/llm/eval/comment-to-code.target")
(functional-eval-one "higher-order"
  "ml/llm/eval/higher-order.prompt" "ml/llm/eval/higher-order.target")
(functional-eval-summary)
