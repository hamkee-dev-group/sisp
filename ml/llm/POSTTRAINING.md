# Native SISP transformer post-training

The complete post-training program is now SISP code. It continues from the
901,856-parameter, 256-context `code-plus` knowledge model and uses the same
native matrix transformer, explicit backward pass, AdamW state, and text
checkpoint representation as pretraining.

There are no environment variables or command-line model options. Edit
`native-posttrain-config.lsp`, then run one fixed batch command:

```sh
# From the repository root
ml/llm/run-native-posttrain.sh
```

The shell file contains no learning policy or model configuration. It only does
the host operations SISP cannot do: enumerate the checkpoint directory, create
the configured output directory, and isolate execution of possibly broken
generated code. SISP supplies the plan, generation, execution check, reward
input, objective, gradients, optimizer update, metrics, and checkpoint content.

## Configuration

All settings live in `native-posttrain-config.lsp`:

- mode: `sft`, `dpo`, or `reinforce`;
- input and output checkpoints;
- prompt, chosen, rejected, check, candidate, and reward files;
- generation count, temperature, top-k, and top-p;
- pass/failure rewards, learning rate, DPO beta, reward baseline, and baseline rate.

The committed configuration is a complete REINFORCE example. SISP generates a
completion for `list-sum`, an isolated SISP process executes its hidden check,
and the main SISP process reads the resulting numeric reward and updates the
transformer.

For SFT, set the mode to `sft`, set `generate-p` to `nil`, and point
`chosen-path` at the desired completion. For DPO, use mode `dpo`, disable
generation, and set both chosen and rejected paths. No shell changes are needed.

The configured output directory must not already exist. This prevents accidental
checkpoint replacement; change `native-posttrain-output` for the next run.

## SISP files

| File | Responsibility |
|---|---|
| `native-posttrain-config.lsp` | Authoritative user configuration |
| `posttrain.lsp` | Masked SFT, DPO, REINFORCE, and distillation mathematics |
| `posttrain-session.lsp` | Completion windows, scoring, backward accumulation, AdamW, metrics |
| `native-posttrain-generate.lsp` | Native transformer sampling and candidate write |
| `native-posttrain-main.lsp` | Mode dispatch and complete learning update |
| `native-posttrain-save.lsp` | Top-level 100-parameter checkpoint schedule |
| `native-posttrain-plan.lsp` | Minimal host-operation plan emitted by SISP |
| `posttrain-native-tests.lsp` | Real forward/backward/update parameter-change test |

The top-level save schedule is deliberately explicit. It lets SISP collect
temporary serialization objects between parameter components instead of retaining
one large recursive save operation.

## Verified end-to-end behavior

The full-size native runs produced:

| Mode | Start/end step | Objective | Gradient norm | Reloaded |
|---|---:|---:|---:|---|
| completion SFT | 2796/2797 | 1.691163426 | 83.536497 | yes |
| DPO | 2796/2797 | 0.693147181 | 112.388524 | yes |
| REINFORCE, reward -0.1 | 2796/2797 | -0.216331165 | 62.982606 | yes |
| config-driven REINFORCE, 32 chars | 2796/2797 | -0.491898618 | 19.337006 | yes |

For the final row, a single configuration-only command performed native model
loading, 32-character generation, SISP execution, reward creation, REINFORCE
backpropagation, AdamW, all 100 checkpoint writes, and native reload. PyTorch was
not involved.

`run-tests.sh` now contains four suites: 19 scalar-autodiff checks, 17 shared LLM
checks, 19 post-training objective checks, and 5 actual native optimizer-update
checks. All pass.

## Completion quality

The path is operational, but the tiny model still generated no successful
functional program in the first reinforcement experiment. A separate fixed-seed
comparison scored 0/32 for both base and updated policies. Uniform failure reward
can suppress sampled failures but cannot show a route to correct code. Future
quality work needs reachable partial rewards; this does not affect the verified
native implementation.
