
exports.clearCanvas = function () {
  var canvas = document.getElementById('thecanvas');
  var context = canvas.getContext('2d');
  console.log("hai")
  context.clearRect(0,0,canvas.width,canvas.height);
};
