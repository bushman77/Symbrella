/*
 * Symbrella complete Brain report export.
 *
 * Intentionally served as a standalone static asset: this feature must work
 * even when the Termux Tailwind or esbuild binaries are unavailable.
 *
 * Use delegated event listeners so Phoenix LiveView can patch the dashboard
 * every 500ms without detaching the export controls.
 */
(function () {
  "use strict";

  const COPY_ID = "brain-report-copy-btn";
  const VIEW_ID = "brain-report-view-btn";
  const ROOT_ID = "brain-report-root";
  const DIALOG_ID = "brain-report-export-dialog";

  function readSection(section, root) {
    const selectors = (section.getAttribute("data-brain-report-source") || "")
      .split(",")
      .map((value) => value.trim())
      .filter(Boolean);

    for (const selector of selectors) {
      const source = root.querySelector(selector);
      if (source) {
        const value = source.value == null ? source.textContent : source.value;
        if (typeof value === "string" && value.trim()) return value.trim();
      }
    }

    return (section.innerText || section.textContent || "").trim() || "(no data)";
  }

  function buildReport(root) {
    if (!root) return null;

    const sections = Array.from(root.querySelectorAll("[data-brain-report-section]"));
    const region = root.querySelector('select[name="region"]');
    const chunks = [
      "Symbrella - Complete Brain Diagnostics",
      "Captured: " + new Date().toISOString(),
      "Selected region: " + (region ? region.value : "unknown"),
      "Sections: " + sections.length
    ];

    for (const section of sections) {
      const title = section.getAttribute("data-brain-report-section") || "Diagnostics";
      chunks.push("\n===== " + title + " =====\n" + readSection(section, root));
    }

    return chunks.join("\n").trim() + "\n";
  }

  // Legacy copy must happen synchronously inside a click handler to retain the
  // user's activation on mobile. Try it first on insecure HTTP connections,
  // where navigator.clipboard is usually unavailable.
  function legacyCopy(report) {
    const field = document.createElement("textarea");
    field.value = report;
    field.readOnly = true;
    field.setAttribute("aria-label", "Temporary Brain report clipboard field");
    field.style.cssText =
      "position:fixed;top:0;left:-9999px;width:1px;height:1px;opacity:0;";

    document.body.appendChild(field);
    try {
      field.focus();
      field.select();
      field.setSelectionRange(0, field.value.length);
      return document.execCommand("copy") === true;
    } catch (_) {
      return false;
    } finally {
      field.remove();
    }
  }

  // Native clipboard is preferred on HTTPS or localhost; never claim success
  // unless the promise resolves. If it is unavailable/denied, show the report.
  async function copyReport(report) {
    if (navigator.clipboard && typeof navigator.clipboard.writeText === "function") {
      try {
        await navigator.clipboard.writeText(report);
        return true;
      } catch (_) {
        return legacyCopy(report);
      }
    }

    return legacyCopy(report);
  }

  function downloadReport(report) {
    const blob = new Blob([report], { type: "text/plain;charset=utf-8" });
    const url = URL.createObjectURL(blob);
    const link = document.createElement("a");
    link.href = url;
    link.download = "symbrella-brain-report-" +
      new Date().toISOString().replace(/[:.]/g, "-") + ".txt";
    document.body.appendChild(link);
    try {
      link.click();
    } finally {
      link.remove();
      setTimeout(() => URL.revokeObjectURL(url), 2000);
    }
  }

  function showReport(report, blocked) {
    const previous = document.getElementById(DIALOG_ID);
    if (previous) previous.remove();

    const overlay = document.createElement("div");
    overlay.id = DIALOG_ID;
    overlay.setAttribute("role", "dialog");
    overlay.setAttribute("aria-modal", "true");
    overlay.setAttribute("aria-label", "Complete Brain report");
    overlay.style.cssText =
      "position:fixed;inset:0;background:rgba(0,0,0,.72);z-index:10000;" +
      "padding:12px;display:flex;align-items:center;justify-content:center;";

    const panel = document.createElement("div");
    panel.style.cssText =
      "width:min(100%,900px);max-height:90vh;display:flex;flex-direction:column;" +
      "gap:10px;border-radius:12px;background:white;color:#111;padding:14px;" +
      "box-shadow:0 10px 30px #0006;";

    const heading = document.createElement("strong");
    heading.textContent = blocked
      ? "Clipboard unavailable - report ready to select or save"
      : "Complete Brain Report";

    const help = document.createElement("p");
    help.style.cssText = "font:14px system-ui;margin:0;";
    help.textContent = blocked
      ? "Your browser blocked automatic copying. Long-press the text to select/copy it, or save it as a .txt file."
      : "All currently retained diagnostics are below. Select the text or save a .txt file.";

    const field = document.createElement("textarea");
    field.value = report;
    field.readOnly = true;
    field.spellcheck = false;
    field.setAttribute("aria-label", "Complete Brain diagnostics text");
    field.style.cssText =
      "width:100%;min-height:240px;height:52vh;resize:vertical;white-space:pre;" +
      "font:12px monospace;background:#f8fafc;color:#111;border:1px solid #aaa;" +
      "border-radius:6px;padding:10px;";

    const actions = document.createElement("div");
    actions.style.cssText = "display:flex;gap:8px;flex-wrap:wrap;justify-content:flex-end;";

    const select = document.createElement("button");
    select.type = "button";
    select.textContent = "Select All";
    select.addEventListener("click", function () {
      field.focus();
      field.select();
      field.setSelectionRange(0, field.value.length);
    });

    const save = document.createElement("button");
    save.type = "button";
    save.textContent = "Download .txt";
    save.addEventListener("click", function () { downloadReport(report); });

    const close = document.createElement("button");
    close.type = "button";
    close.textContent = "Close";
    close.addEventListener("click", function () { overlay.remove(); });

    for (const button of [select, save, close]) {
      button.style.cssText =
        "padding:9px 14px;border:1px solid #777;border-radius:7px;" +
        "background:#f1f5f9;color:#111;font:14px system-ui;cursor:pointer;";
      actions.appendChild(button);
    }

    panel.append(heading, help, field, actions);
    overlay.appendChild(panel);
    document.body.appendChild(overlay);
    field.focus();
    field.select();
  }

  function markButton(button, message) {
    if (!button) return;

    // LiveView refreshes the header frequently. Keep feedback outside of the
    // patched tree so the user can see whether copying actually succeeded.
    const oldToast = document.getElementById("brain-report-feedback");
    if (oldToast) oldToast.remove();

    const toast = document.createElement("div");
    toast.id = "brain-report-feedback";
    toast.setAttribute("role", "status");
    toast.textContent = message;
    toast.style.cssText =
      "position:fixed;bottom:18px;left:50%;transform:translateX(-50%);" +
      "z-index:10001;border-radius:8px;padding:10px 16px;background:#111827;" +
      "color:white;font:14px system-ui;box-shadow:0 4px 20px #0005;";
    document.body.appendChild(toast);
    setTimeout(function () { toast.remove(); }, 3000);

    const label = button.querySelector("[data-brain-copy-label]");
    if (label) label.textContent = message;
    clearTimeout(button._brainReportReset);
    button._brainReportReset = setTimeout(function () {
      const fresh = document.getElementById(COPY_ID);
      const freshLabel = fresh && fresh.querySelector("[data-brain-copy-label]");
      if (freshLabel) freshLabel.textContent = "Copy Full Brain Report";
    }, 2500);
  }

  document.addEventListener("click", async function (event) {
    const button = event.target.closest("#" + COPY_ID + ", #" + VIEW_ID);
    if (!button) return;

    event.preventDefault();

    try {
      const report = buildReport(document.getElementById(ROOT_ID));
      if (!report) {
        markButton(button, "Report unavailable");
        alert("Brain report is unavailable. Refresh the page and try again.");
        return;
      }

      if (button.id === VIEW_ID) {
        showReport(report, false);
        return;
      }

      // Handle one click at a time. The report is already captured at click.
      if (button.dataset.brainCopyBusy === "1") return;
      button.dataset.brainCopyBusy = "1";

      try {
        const copied = await copyReport(report);
        markButton(button, copied ? "Copied!" : "Open report to copy");
        if (!copied) showReport(report, true);
      } finally {
        button.dataset.brainCopyBusy = "0";
      }
    } catch (error) {
      markButton(button, "Open report to copy");
      const report = buildReport(document.getElementById(ROOT_ID));
      if (report) showReport(report, true);
    }
  });

  // Visible in browser developer tools for diagnosing stale/missing assets.
  window.SymbrellaBrainReport = { buildReport: buildReport };
})();
