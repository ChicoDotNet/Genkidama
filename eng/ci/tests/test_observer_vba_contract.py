from __future__ import annotations

import re
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
SOURCE = ROOT / "src/Shell/VBA/ObserverExample.bas"


class ObserverVbaContractTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.text = SOURCE.read_text(encoding="utf-8")

    def test_module_has_strict_entrypoint_and_subscription_lifecycle(self) -> None:
        self.assertRegex(self.text, r"(?im)^Option Explicit$")
        self.assertRegex(
            self.text,
            r"(?im)^Public Function ObserverExamplePasses\(\) As Boolean$",
        )
        self.assertIn("Set mSubscribers = New Collection", self.text)
        self.assertIn("mSubscribers.Add handlerName, handlerName", self.text)
        self.assertIn("mSubscribers.Remove handlerName", self.text)
        self.assertIn("If Err.Number = 457 Then", self.text)

    def test_publish_dispatches_through_registered_handler_contract(self) -> None:
        match = re.search(
            r"(?ims)^Private Sub Publish\(ByVal eventValue As String\)(.*?)^End Sub$",
            self.text,
        )
        self.assertIsNotNone(match)
        body = match.group(1)
        self.assertIn("For Each handlerName In mSubscribers", body)
        self.assertIn("Application.Run CStr(handlerName), eventValue", body)
        self.assertNotIn("AuditObserver", body)
        self.assertNotIn("DashboardObserver", body)

    def test_example_proves_multiple_observers_duplicate_rejection_and_unsubscribe(self) -> None:
        self.assertIn('Subscribe("AuditObserver")', self.text)
        self.assertIn('Subscribe("DashboardObserver")', self.text)
        self.assertIn('If Subscribe("AuditObserver") Then Exit Function', self.text)
        self.assertIn('If Not Unsubscribe("DashboardObserver") Then Exit Function', self.text)
        self.assertIn('If Unsubscribe("DashboardObserver") Then Exit Function', self.text)
        self.assertIn('If mAuditLog.Count <> 2 Then Exit Function', self.text)
        self.assertIn('If mDashboardLog.Count <> 1 Then Exit Function', self.text)


if __name__ == "__main__":
    unittest.main()
