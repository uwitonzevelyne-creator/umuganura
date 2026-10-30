(function () {
  function syncOpenClass(box) {
    if (!box) return;
    box.classList.toggle("is-open", !!box.open);
  }

  document.addEventListener("toggle", function (e) {
    var box = e.target;
    if (!box || !box.classList || !box.classList.contains("about-acc")) return;
    syncOpenClass(box);
    if (!box.open) return;
    document.querySelectorAll("#about .about-acc[open]").forEach(function (other) {
      if (other !== box) {
        other.open = false;
        syncOpenClass(other);
      }
    });
  }, true);

  document.querySelectorAll("#about .about-acc").forEach(syncOpenClass);
})();
