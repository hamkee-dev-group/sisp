# Held-out agent policy fixtures

These 12 two-turn episodes are evaluation data. Each `.txt` file contains two
exact runtime prompt/action pairs: a tool or recovery action followed by a final
answer. Their complete prompts and goal/constant combinations are intentionally
absent from the agent policy training fixtures.

Do not add this directory to any corpus input. Run
`ml/agent/run-protocol-tests.sh` to validate all 24 held-out targets.

After training, evaluate a checkpoint with the same profile and corpus contract
used to create it:

```sh
SISP_LLM_PROFILE=code-plus \
SISP_LLM_RESUME=/path/to/checkpoint \
SISP_LLM_CORPUS=/path/to/train.txt \
SISP_LLM_VALID_CORPUS=/path/to/valid.txt \
ml/agent/run-policy-eval.sh
```

The evaluator reports teacher-forced character NLL/top-1/top-5 and greedy exact
match for all 24 targets. It also executes the first balanced generated action
and reports parsing, correct single-tool selection, and semantic observation.
Model failures are scores; missing files or broken evaluation infrastructure
remain fatal.
