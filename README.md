Welcome to This Mini_Projects Portfolio ^--^

Each file is presented as the final work and presentations to the class.

If interested in any details, they are available upon requests.

Thank you for viewing! <^-^>

<!DOCTYPE html>
<html>
<head>
  <title>Cesium Globe</title>
  <script src="https://cesium.com/downloads/cesiumjs/releases/1.85/Build/Cesium/Cesium.js"></script>
  <link href="https://cesium.com/downloads/cesiumjs/releases/1.85/Build/Cesium/Widgets/widgets.css" rel="stylesheet">
  <style>
    #cesiumContainer {
      width: 100%;
      height: 100vh;
      margin: 0;
      padding: 0;
    }
  </style>
</head>
<body>
  <div id="cesiumContainer"></div>
  <script>
    var viewer = new Cesium.Viewer('cesiumContainer');
    viewer.scene.globe.enableLighting = true;

    // Highlight US and Australia
    viewer.entities.add({
      name: 'US',
      polygon: {
        hierarchy: Cesium.Cartesian3.fromDegreesArray([
          -125, 24, -66, 24, -66, 50, -125, 50
        ]),
        material: Cesium.Color.RED.withAlpha(0.5)
      }
    });

    viewer.entities.add({
      name: 'Australia',
      polygon: {
        hierarchy: Cesium.Cartesian3.fromDegreesArray([
          113, -44, 153, -44, 153, -10, 113, -10
        ]),
        material: Cesium.Color.BLUE.withAlpha(0.5)
      }
    });

    // Spin the globe
    viewer.scene.preRender.addEventListener(function (scene, time) {
      viewer.camera.lookAtTransform(Cesium.Matrix4.IDENTITY);
      viewer.camera.rotateRight(0.01);
    });
  </script>
</body>
</html>

