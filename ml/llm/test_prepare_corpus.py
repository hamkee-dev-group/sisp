#!/usr/bin/env python3
"""Fast regression checks for family-safe corpus preparation."""

from __future__ import annotations

import importlib.util
import sys
import tempfile
import unittest
from pathlib import Path


MODULE_PATH = Path(__file__).with_name("prepare-corpus.py")
SPEC = importlib.util.spec_from_file_location("prepare_corpus", MODULE_PATH)
assert SPEC and SPEC.loader
prepare = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = prepare
SPEC.loader.exec_module(prepare)


def unit(name: str, family: str, text: str) -> object:
    import hashlib
    payload = text.encode()
    digest = hashlib.sha256(payload).hexdigest()
    return prepare.Unit(name, text, digest, len(payload), digest, family,
                        "test", "test", tuple())


class CorpusPreparationTests(unittest.TestCase):
    def test_known_math_symbols_are_transliterated_in_comments_and_strings(self) -> None:
        text, transformations = prepare.normalize(
            "; A ∪ B → union\n(define (f x) \"keep-λ and A ⊆ B\")\n", "balanced"
        )
        self.assertIn("A union B -> union", text)
        self.assertIn('"keep-lambda and A subset B"', text)
        self.assertIn("ascii-symbol-transliteration", transformations)
        vocabulary = set(Path(__file__).with_name("vocab-code.txt").read_text())
        self.assertFalse(set(text) - vocabulary)

    def test_accented_latin_text_is_transliterated_without_dropping_unit(self) -> None:
        text, transformations = prepare.normalize(
            '; una città con caffè\n(print "déjà vu")\n', "balanced"
        )
        self.assertIn("una citta con caffe", text)
        self.assertIn('"deja vu"', text)
        self.assertIn("ascii-compatibility-transliteration", transformations)
        vocabulary = set(Path(__file__).with_name("vocab-code.txt").read_text())
        accepted, excluded = prepare.enforce_vocabulary(
            [unit("accented.lsp", "accented", text)], "".join(vocabulary), "drop"
        )
        self.assertEqual(len(accepted), 1)
        self.assertEqual(excluded, [])

    def test_untranslatable_source_character_is_not_silently_deleted(self) -> None:
        text, _ = prepare.normalize("(define 猫 1)\n", "keep")
        self.assertIn("猫", text)

    def test_source_metadata_uses_location_without_inferring_authorship(self) -> None:
        self.assertEqual(
            prepare.unit_metadata("ml/training/seed/search-variant-2.lsp"),
            ("search", "training-seed", "unspecified"),
        )
        self.assertEqual(
            prepare.unit_metadata("ml/training/gen/search-sample-3.lsp"),
            ("search", "generated", "unspecified"),
        )
        self.assertEqual(
            prepare.unit_metadata("examples/search.lsp"),
            ("search", "repository-example", "unspecified"),
        )

    def test_related_families_never_cross_splits(self) -> None:
        units = [
            unit("a-first.lsp", "same", "(define (a x) (+ x 1))\n" * 20),
            unit("a-second.lsp", "same", "(define (b x) (+ x 2))\n" * 20),
            unit("c.lsp", "c", "(define (c x) (* x 3))\n" * 20),
            unit("d.lsp", "d", "(define (d x) (- x 4))\n" * 20),
        ]
        splits = prepare.split_units(units, [], 7, 0.60, 0.20)
        locations = {
            item.path: split for split, members in splits.items() for item in members
        }
        self.assertEqual(locations["a-first.lsp"], locations["a-second.lsp"])

    def test_forced_training_prefix_moves_whole_related_group(self) -> None:
        units = [
            unit("ml/agent/protocol/train/a.txt", "same", "(a)\n" * 20),
            unit("related.txt", "same", "(b)\n" * 20),
            unit("c.txt", "c", "(c)\n" * 20),
            unit("d.txt", "d", "(d)\n" * 20),
        ]
        splits = prepare.split_units(
            units, [], 7, 0.60, 0.20, ("ml/agent/protocol/train/",)
        )
        self.assertIn(units[0], splits["train"])
        self.assertIn(units[1], splits["train"])

    def test_span_files_exclude_separator_bytes(self) -> None:
        units = [
            unit("a.lsp", "a", "(define (a x) x)\n" * 20),
            unit("b.lsp", "b", "(define (b x) x)\n" * 20),
            unit("c.lsp", "c", "(define (c x) x)\n" * 20),
        ]
        args = type("Args", (), {
            "seed": 1, "comments": "balanced", "near_duplicate_threshold": 0.72,
            "fim_rate": 0.0, "fim_seed": 1,
        })()
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory)
            splits = {"train": [units[0]], "valid": [units[1]], "test": [units[2]]}
            prepare.write_outputs(output, splits, [], [], units, [],
                                  Path(__file__).with_name("vocab-code.txt").read_text(), args)
            expected_length = len(units[0].text.rstrip())
            self.assertIn(f"[[0 {expected_length} 0]]",
                          (output / "train-spans.lsp").read_text())

    def test_fim_is_deterministic_and_training_only(self) -> None:
        sample = unit("a.lsp", "a", "one\ntwo\nthree\nfour\nfive\n")
        args = type("Args", (), {"fim_rate": 1.0, "fim_seed": 11})()
        first, changed = prepare.fim_text(sample, args)
        second, changed_again = prepare.fim_text(sample, args)
        self.assertTrue(changed and changed_again)
        self.assertEqual(first, second)
        self.assertEqual(first.count("<fim-"), 3)

    def test_forced_policy_data_is_never_reordered_by_fim(self) -> None:
        sample = unit(
            "ml/agent/protocol/train/a.txt", "a",
            "one\ntwo\nthree\nfour\nfive\n",
        )
        args = type("Args", (), {
            "fim_rate": 1.0, "fim_seed": 11,
            "force_train_prefix": ["ml/agent/protocol/train/"],
        })()
        transformed, changed = prepare.fim_text(sample, args)
        self.assertFalse(changed)
        self.assertEqual(transformed, sample.text.rstrip())


if __name__ == "__main__":
    unittest.main(verbosity=2)
