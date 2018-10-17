module Core.InventoryPage where

import qualified Brick.Widgets.List as L

import Core.Equipment
import Core.Utils(CursorName)

data InventoryPage = InventoryPage{
  allEquipments :: L.List CursorName Equipment
  }

invPageMoveUp :: InventoryPage -> InventoryPage
invPageMoveUp InventoryPage{
  allEquipments = inv
  } = InventoryPage{
  allEquipments = L.listMoveUp inv}

invPageMoveDown :: InventoryPage -> InventoryPage
invPageMoveDown InventoryPage{
  allEquipments = inv
  } = InventoryPage{
  allEquipments = L.listMoveDown inv}

