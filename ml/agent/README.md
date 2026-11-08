# Tiny SISP research agent

This directory turns the trained character transformer in `ml/llm`
into a real, deliberately tiny agent. The control loop is:

1. Read a goal, the previous observation, and persistent memory.
2. Ask the transformer to generate one SISP action.
3. Write the generated characters to `generated/current/action.lsp`.
4. Let SISP process that file as its next top-level input.
5. Record the tool result as the next observation and repeat.
6. Stop on `agent-answer` or at the turn budget.

The model, state, actions, tool functions, memory, and observations all live in
SISP. `run-agent.sh` only performs mechanical startup: it builds the same model
configuration used for training and orders the top-level input files. No change
to the C interpreter is required, and no `read-from-string` primitive is needed.

## What it can do

The generated action may use these compact tools:

- `(agent-eval 'FORM)` evaluates a SISP form and returns its result as an
  observation.
- `(agent-remember KEY VALUE)` appends persistent text memory.
- `(agent-recall KEY)` retrieves a small memory window.
- `(agent-think TEXT)` spends a turn retaining a short intermediate thought.
- `(agent-answer VALUE)` produces the final answer and stops.

Generation stops after the first balanced SISP form, and an action is accepted
only when exactly one agent tool is invoked. An early answer clears the already
scheduled next action slot, so completed actions cannot be replayed.

`agent-eval` is intentionally general. Generated code can call ordinary SISP
functions, including list/matrix operations and file primitives. It therefore
has the same filesystem authority as the interpreter. This is a research
runner, not a sandbox: malformed code may error, loop, overwrite files, or quit.

The agent has no network or process access because SISP has neither. Persistent
memory is an append-only text file with latest-exact-key recall; the transcript records every action and
observation. This is the honest agent analogue of a real LLM agent, but at toy
scale: model policy, action, tool result, memory, and another policy step.

## Run it

First put a plain-language goal in a text file. Then select the checkpoint's
profile and checkpoint directory:

```sh
SISP_LLM_PROFILE=code \
SISP_LLM_RESUME=ml/llm/runs/my-run/latest \
SISP_AGENT_GOAL_FILE=/tmp/goal.txt \
ml/agent/run-agent.sh
```

Useful controls are:

```text
SISP_AGENT_TURNS=3
SISP_AGENT_ACTION_CHARS=96
SISP_AGENT_TEMPERATURE=0.3
SISP_AGENT_TOP_K=5
SISP_AGENT_TOP_P=0.95
SISP_AGENT_TIMEOUT_SECONDS=1800
SISP_AGENT_MEMORY=ml/agent/memory/default.txt
SISP_AGENT_TRANSCRIPT=ml/agent/generated/current/transcript.txt
```

The `code` profile (479,552 parameters, context 256) is the primary training
target. `code-plus` (901,856 parameters, context 256) is the equal-character
challenger. A checkpoint must match its profile, vocabulary, corpus, and training
settings when training resumes. Agent inference validates checkpoint integrity,
architecture, vocabulary, and finite parameters without loading the training or
validation corpus.

For an execution-path smoke test with random weights:

```sh
SISP_LLM_PROFILE=small \
SISP_AGENT_ALLOW_RANDOM=1 \
SISP_AGENT_GOAL_FILE=ml/agent/protocol-fixture.txt \
SISP_AGENT_TURNS=1 \
SISP_AGENT_ACTION_CHARS=8 \
ml/agent/run-agent.sh
```

Random weights should produce nonsense. This command only proves the complete
model-to-text-to-SISP-to-observation path.

Agent prompts use their true length rather than space padding. Goal, observation,
and memory fields are forced onto one comment line so recalled text cannot forge a
new `;A:` or `;O:` protocol field. The runner exits with status 124 on its
configurable wall-clock limit and exits nonzero if generated actions produced
interpreter diagnostics, even if SISP continued to the final report.

## Training requirement

The base code model will not reliably invent the action protocol. The corpus
builder force-assigns the 64 exact episodes in `protocol/train/` to training and
never applies fill-in-the-middle reordering to them. They cover all five actions,
tool results, memory, final answers, and recovery after invalid or unhelpful
observations. The 12 episodes in `eval/` are permanently held out.

After each candidate checkpoint, measure those 24 unseen actions without
changing the model:

```sh
SISP_LLM_PROFILE=code \
SISP_LLM_CORPUS=ml/llm/data/local/sisp-v1/train.txt \
SISP_LLM_VALID_CORPUS=ml/llm/data/local/sisp-v1/valid.txt \
SISP_LLM_RESUME=/srv/sisp/checkpoints/code.best \
  ml/agent/run-policy-eval.sh
```

It reports teacher-forced NLL/top-1/top-5, greedy exact actions, parse success,
correct single-tool selection, and semantic observation success. The command
requires at least one semantic held-out success by default and exits nonzero
otherwise. Set `SISP_AGENT_POLICY_MIN_SEMANTIC` to a stricter promotion gate.

This is policy imitation (next-character gradient training), not reinforcement
learning. The architecture remains the same genuine transformer; only the
training examples teach it to behave as an agent.

## Tests

```sh
ml/agent/run-tests.sh
ml/agent/run-protocol-tests.sh
ml/agent/run-external-tests.sh
```

## External model backend

`external-agent.py` connects the same SISP agent state machine to any model
available as a command-line adapter.  The adapter receives the prompt on
standard input and must write exactly one SISP action on standard output:

```text
(agent-eval 'FORM)
(agent-recall KEY)
(agent-remember KEY VALUE)
(agent-think TEXT)
(agent-answer VALUE)
```

Run it from the repository root:

```sh
python3 ml/agent/external-agent.py \
  --goal-file /path/to/goal.txt \
  --model-command '/path/to/model-adapter --model MODEL'
```

The command is parsed into arguments and executed directly, without a shell.
It may be a local model executable or a small adapter for a remote service.
Provider authentication, HTTP, and JSON stay in that adapter; they are not SISP
runtime primitives.

For an expensive model, `--persistent-model-command` keeps the adapter alive
across turns. It reads one JSON object per line with a string `prompt` field and
writes one JSON object per line with a string `completion` field. This changes
only transport performance; SISP retains the same ownership and validation.

The driver is only transport and process supervision.  The persistent SISP
process constructs every prompt, validates direct single-tool actions, executes
tools, produces observations, maintains memory and turn budgets, and writes the
transcript.  Requests and responses are retained under
`ml/agent/generated/external/` by default.  Use `--workspace` to select a
different location and `--reset-memory` to start without previous memory.
The driver exits with status 3 when the SISP turn budget is exhausted without
an `agent-answer`, so an incomplete run cannot be mistaken for success.

This is not a sandbox.  `agent-eval` intentionally evaluates SISP with the
interpreter's filesystem authority.  The external boundary rejects prose,
multiple forms, wrappers such as `progn`, and syntax containing more than one
agent tool, but accepted `agent-eval` code remains trusted research-agent code.

The deterministic suite checks prompt construction, real newline handling,
memory, evaluation, final answers, and the actual generated-file execution
path. The protocol gate validates all 152 prompt/action targets through SISP,
field/context budgets, frozen vocabulary, action distribution, and held-out
separation. Generated actions, transcripts, memories, and model checkpoints are
ignored by Git.
