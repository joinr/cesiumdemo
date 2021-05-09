(ns cesiumdemo.picking)

;; /**
;;  * Returns the top-most Entity at the provided window coordinates
;;  * or undefined if no Entity is at that location.
;;  *
;;  * @param {Cartesian2} windowPosition The window coordinates.
;;  * @returns {Entity} The picked Entity or undefined.
;;  */
;; function pickEntity(viewer, windowPosition) {
;;   var picked = viewer.scene.pick(windowPosition);
;;   if (defined(picked)) {
;;     var id = Cesium.defaultValue(picked.id, picked.primitive.id);
;;     if (id instanceof Cesium.Entity) {
;;       return id;
;;     }
;;   }
;;   return undefined;
;; };

;; /**
;;  * Returns the list of Entities at the provided window coordinates.
;;  * The Entities are sorted front to back by their visual order.
;;  *
;;  * @param {Cartesian2} windowPosition The window coordinates.
;;  * @returns {Entity[]} The picked Entities or undefined.
;;  */
;; function drillPickEntities(viewer, windowPosition) {
;;   var i;
;;   var entity;
;;   var picked;
;;   var pickedPrimitives = viewer.scene.drillPick(windowPosition);
;;   var length = pickedPrimitives.length;
;;   var result = [];
;;   var hash = {};

;;   for (i = 0; i < length; i++) {
;;     picked = pickedPrimitives[i];
;;     entity = Cesium.defaultValue(picked.id, picked.primitive.id);
;;     if (entity instanceof Cesium.Entity &&
;;         !Cesium.defined(hash[entity.id])) {
;;       result.push(entity);
;;       hash[entity.id] = true;
;;     }
;;   }
;;   return result;
;; };
