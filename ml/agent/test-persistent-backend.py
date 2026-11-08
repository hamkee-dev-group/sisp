#!/usr/bin/env python3
"""Transport checks for the long-lived external model protocol."""

from __future__ import annotations

import importlib.util
import sys
import unittest
from pathlib import Path


MODULE_PATH = Path(__file__).with_name("external-agent.py")
SPEC = importlib.util.spec_from_file_location("external_agent", MODULE_PATH)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError("cannot load external-agent.py")
external_agent = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(external_agent)


class PersistentBackendTests(unittest.TestCase):
    def test_process_is_reused_and_whitespace_reaches_sisp_validation(self) -> None:
        program = (
            "import json,sys\n"
            "count=0\n"
            "for line in sys.stdin:\n"
            " count+=1\n"
            " request=json.loads(line)\n"
            " completion='\\n' if request['prompt']=='space' else f'(agent-answer {count})'\n"
            " print(json.dumps({'completion':completion}),flush=True)\n"
        )
        model = external_agent.PersistentModel(
            [sys.executable, "-u", "-c", program], timeout=5, maximum=1024
        )
        try:
            self.assertEqual(model.complete("first"), "(agent-answer 1)")
            self.assertEqual(model.complete("second"), "(agent-answer 2)")
            self.assertEqual(model.complete("space"), "\n")
        finally:
            model.close()


if __name__ == "__main__":
    unittest.main()
