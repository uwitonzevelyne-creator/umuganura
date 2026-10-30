/* Keep Leaflet maps sized and inside their card when panels/steps/tabs reveal. */
(function () {
  function widgetsIn(root) {
    if (!root || !root.querySelectorAll) return [];
    if (root.nodeType === 1 && root.classList && root.classList.contains("leaflet") && root.classList.contains("html-widget")) {
      return [root];
    }
    return Array.prototype.slice.call(root.querySelectorAll(".leaflet.html-widget"));
  }

  function invalidateOne(el) {
    if (!el || !el.id || !window.HTMLWidgets) return;
    try {
      var w = window.HTMLWidgets.find("#" + el.id);
      if (w && w.getMap) {
        var map = w.getMap();
        if (map && map.invalidateSize) map.invalidateSize(false);
      }
    } catch (err) {}
  }

  function invalidateLeaflets(root) {
    widgetsIn(root || document).forEach(invalidateOne);
  }

  function invalidateSoon(root) {
    invalidateLeaflets(root);
    setTimeout(function () { invalidateLeaflets(root); }, 90);
    setTimeout(function () { invalidateLeaflets(root); }, 320);
  }

  window.UmuganuraMaps = { invalidate: invalidateSoon, invalidateNow: invalidateLeaflets };

  if (window.jQuery) {
    window.jQuery(document).on("shown", function (e) {
      invalidateSoon(e.target || document);
    });
  }

  document.addEventListener("shiny:value", function (e) {
    var el = e.target;
    if (el && el.classList && el.classList.contains("leaflet") && el.classList.contains("html-widget")) {
      invalidateSoon(el);
    }
  });
})();
