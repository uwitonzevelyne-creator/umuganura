(function () {
  var crops = [
    { src: "hero/cut/plant-avocado.png", name: "Avocado" },
    { src: "hero/cut/plant-bean.png", name: "Bean" },
    { src: "hero/cut/plant-maize.png", name: "Maize" },
    { src: "hero/cut/plant-rice.png", name: "Rice" },
    { src: "hero/cut/plant-sorghum.png", name: "Sorghum" },
    { src: "hero/cut/plant-coffee.png", name: "Coffee" },
    { src: "hero/cut/plant-banana.png", name: "Banana" }
  ];
  var img = document.getElementById("stage-crop");
  if (!img) return;
  var i = 0;

  crops.forEach(function (c) {
    var preload = new Image();
    preload.src = c.src;
  });

  function show(index) {
    var crop = crops[index];
    img.classList.add("is-fading");
    window.setTimeout(function () {
      img.src = crop.src;
      img.alt = crop.name;
      img.classList.remove("is-fading");
    }, 280);
  }

  show(0);
  window.setInterval(function () {
    i = (i + 1) % crops.length;
    show(i);
  }, 4000);
})();
