export const HeaderSizer = {
  mounted() { this.resize(); window.addEventListener("resize", this.resize); },
  destroyed() { window.removeEventListener("resize", this.resize); },
  resize: () => {
    const el = document.getElementById("chat-header");
    const h  = el ? el.offsetHeight : 56;
    document.documentElement.style.setProperty("--hdr", `${h}px`);
  }
};

export const FooterSizer = {
  mounted() { this.resize(); window.addEventListener("resize", this.resize); },
  destroyed() { window.removeEventListener("resize", this.resize); },
  resize: () => {
    const el = document.getElementById("chat-composer");
    const h  = el ? el.offsetHeight : 72;
    document.documentElement.style.setProperty("--ftr", `${h}px`);
  }
};

export const AutoScroll = {
  mounted() {
    this.scroll();
    // Handle server push_event("chat:scroll", %{})
    this.handleEvent("chat:scroll", () => this.scroll(true));
  },
  updated() { this.scroll(); },
  scroll(smooth = true) {
    const bottom = this.el.querySelector("#bottom");
    if (!bottom) return;
    bottom.scrollIntoView({ behavior: smooth ? "smooth" : "auto", block: "end" });
  }
};

export const ChatInput = {
  mounted() {
    this.el.addEventListener("keydown", (e) => {
      if (e.key === "Enter" && !e.shiftKey) {
        e.preventDefault();
        // submit the closest form
        const form = this.el.closest("form");
        if (form) form.requestSubmit ? form.requestSubmit() : form.submit();
      }
    });
  }
};

