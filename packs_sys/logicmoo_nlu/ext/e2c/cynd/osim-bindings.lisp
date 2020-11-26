

(sim-assert '(#$siblingDisjointExceptions #$BPVItem #$FictionalThing) *MappingMt*)
(sim-assert '(#$siblingDisjointExceptions #$MythologicalThing #$SpatialThing) *MappingMt*)

#|

(fi-kill (find-or-create-constant "Merman"))

(sim-assert `(#$simGenls #$SpatialThing-Localized (#$SimFacetFn "rezclass" "idMoveable")) *MappingMt*)
(sim-assert `(#$simGenls #$PhysicalPartOfObject (#$SimFacetFn "rezclass" "idAFAttachment")) *MappingMt*)
(sim-assert `(#$simGenls #$Doorway (#$SimFacetFn "rezclass" "idDoor"))  *MappingMt*)
(sim-assert '(#$simGenls #$Agent-Generic (#$SimFacetFn "rezclass" "idAI")) *MappingMt*)

;;(#$and (#$simGenls ?COL ?FACET1)(#$isa ?COL #$FacetInstanceCollection)(#$isa ?COL ?COLTYPE)(#$isa ?COLTYPE FacetingCollectionType)(#$disjointWith ?COL ?DISJ)(#$simGenls ?DISJ ?FACET2))
;;(#$disjointWith ?FACET1 ?FACET2)

;;(sim-assert '(#$implies (#$and (#$simGenls ?COL1 ?FACET1)(#$simGenls ?COL2 ?FACET2)(#$disjointWith ?COL1 ?COL2))(#$disjointWith ?FACET1 ?FACET2))*MappingMt*)


;;(sim-assert `(#$simGenls #$Path-Simple (#$SimFacetFn "rezclass" "idDoor")) *MappingMt*)
(sim-assert '(#$simGenls #$PartiallyTangible (#$SimFacetFn "rezclass" "idMoveable")) *MappingMt*)
;;(sim-assert '(#$simGenls #$PartiallyTangible (#$SimFacetFn "rezclass" "idAnimatedEntity")) *MappingMt*)
;;(sim-assert '(#$simGenls #$GeographicalPlace-0D (#$SimFacetFn "rezclass" "idPlayerStart")) *MappingMt*)
(sim-assert '(#$simGenls #$BPVLocation (#$SimFacetFn  "classname" "mud_room") ) *MappingMt*)
(sim-assert '(#$simGenls #$BPVAgent (#$SimClassFn "mud_agent") ) *MappingMt*)
(sim-assert '(#$simGenls #$BPVArtifact (#$SimClassFn "mud_item") ) *MappingMt*)
(sim-assert '(#$simGenls #$Handgun (#$SimFacetFn  "classname" "moveable_item_pistol") ) *MappingMt*)
(sim-assert '(#$simGenls #$Rifle (#$SimFacetFn  "classname" "moveable_item_plasmagun") ) *MappingMt*)
(sim-assert '(#$simGenls #$FemaleAnimal (#$SimFacetFn  "classname" "female_animal") ) *MappingMt*)
(sim-assert '(#$simGenls #$MaleAnimal (#$SimFacetFn  "classname" "male_animal") ) *MappingMt*)
(sim-assert '(#$disjointWith (#$SimFacetFn  "classname" "female_animal")(#$SimFacetFn  "classname" "male_animal") ) *MappingMt*)
(sim-assert '(#$disjointWith (#$SimClassFn  "female_animal")(#$SimClassFn "male_animal") ) *MappingMt*)

(fi-kill (find-or-create-constant "Merman"))

(sim-assert '(#$simGenls #$BPVAgent (#$SimClassFn "mud_agent") ) *MappingMt*)
(sim-assert '(#$simGenls #$MaleAnimal (#$SimClassFn "male_animal") ) *MappingMt*)
(sim-assert '(#$simGenls #$FemaleAnimal (#$SimClassFn "female_animal") ) *MappingMt*)

(sim-assert '(#$simGenls #$MaleAnimal (#$SimFacetFn  "classname" "male_animal") ) *MappingMt*)
(sim-assert '(#$genls #$MaleAnimal (#$SimFacetFn  "classname" "male_animal") ) *VocabularyMt*)
(sim-assert '(#$simGenls #$FemaleAnimal (#$SimFacetFn  "classname" "female_animal") ) *MappingMt*)

(sim-assert '(#$genls (#$SimClassFn  "female_animal")(#$SimClassFn "mud_agent") ) *MappingMt*)
(sim-assert '(#$genls (#$SimClassFn  "male_animal")(#$SimClassFn "mud_agent") ) *MappingMt*)

(sim-assert '(#$simGenls #$Monster (#$SimClassFn "monster_demon_trite")) *MappingMt*)
(sim-assert `(#$simGenls #$HomoSapiens (#$SimClassFn "player_simmarine_mp")) *MappingMt*)
(sim-assert '(#$simGenls #$Scientist (#$SimFacetFn "skin" "skins/characters/npcs/body_labcoat_lantern.skin")) *MappingMt*)
(sim-assert '(#$simGenls #$Wrench (#$SimClassFn "moveable_wrench")) *MappingMt*)
(sim-assert '(#$simGenls #$WaxedCardboardCarton (#$SimClassFn "moveable_cartonbox1")) *MappingMt*)
(sim-assert '(#$simGenls #$Wastebasket (#$SimClassFn "moveable_trashcan01")) *MappingMt*)
(sim-assert '(#$simGenls #$Wastebasket (#$SimClassFn "moveable_trashcan")) *MappingMt*)
(sim-assert '(#$simGenls #$Vial (#$SimClassFn "moveable_beaker")) *MappingMt*)
(sim-assert '(#$simGenls #$Vest-Bulletproof (#$SimClassFn "item_armor_shard_mp")) *MappingMt*)
(sim-assert '(#$simGenls #$VerticalFileCabinet-PieceOfFurniture (#$SimClassFn "moveable_filecabinet1")) *MappingMt*)
(sim-assert '(#$simGenls #$Terrorist (#$SimClassFn "human_marine_machinegun")) *MappingMt*)
(sim-assert '(#$simGenls #$Table-PieceOfFurniture (#$SimClassFn "moveable_Table_centercart1")) *MappingMt*)
(sim-assert '(#$simGenls #$TIPersonalComputer (#$SimClassFn "moveable_pc1")) *MappingMt*)
(sim-assert '(#$simGenls #$SwivelChair (#$SimClassFn "moveable_chair1")) *MappingMt*)
(sim-assert '(#$simGenls #$SpiralStaircase (#$SimClassFn "mud_twisty")) *MappingMt*)
(sim-assert '(#$simGenls #$SpatialThing-Localized (#$SimClassFn "moveable_base_fixed")) *MappingMt*)
(sim-assert '(#$simGenls #$Shotgun (#$SimClassFn "weapon_shotgun_mp")) *MappingMt*)
(sim-assert '(#$simGenls #$ShoppingCart (#$SimClassFn "moveable_tablecart2")) *MappingMt*)
(sim-assert '(#$simGenls #$ServingTray (#$SimClassFn "moveable_tray")) *MappingMt*)
(sim-assert '(#$simGenls #$ScrapMetal (#$SimClassFn "debris_barrelpiece2")) *MappingMt*)
(sim-assert '(#$simGenls #$ScrapMetal (#$SimClassFn "debris_barrelpiece")) *MappingMt*) 
(sim-assert '(#$simGenls #$Scientist (#$SimClassFn "alphalabs2_scientist1")) *MappingMt*)
(sim-assert '(#$simGenls #$SafeTheLocker (#$SimClassFn "moveable_metalbox1")) *MappingMt*)
(sim-assert '(#$simGenls #$PropositionalConceptualWork (#$SimClassFn "world")) *MappingMt*)
(sim-assert '(#$simGenls #$ProjectileShell-Blast (#$SimClassFn "ammo_rockets_small_mp")) *MappingMt*)
(sim-assert '(#$simGenls #$ProjectileShell-Blast (#$SimClassFn "ammo_rockets_large_mp")) *MappingMt*)
(sim-assert '(#$simGenls #$PlasticJar (#$SimClassFn "moveable_plasticjar1")) *MappingMt*)
(sim-assert '(#$simGenls #$PlasticFoodContainer (#$SimClassFn "moveable_burgerboxopen")) *MappingMt*)
(sim-assert '(#$simGenls #$PlasticFoodContainer (#$SimClassFn "moveable_burgerboxclose")) *MappingMt*)
(sim-assert '(#$simGenls #$PlasticBox (#$SimClassFn "moveable_plasticbinmini")) *MappingMt*)
(sim-assert '(#$simGenls #$Person (#$SimClassFn "mud_agent")) *MappingMt*)
(sim-assert '(#$simGenls #$Paper (#$SimClassFn "moveable_paperwad")) *MappingMt*)
(sim-assert '(#$simGenls #$Mop (#$SimClassFn "moveable_mop")) *MappingMt*)
(sim-assert '(#$simGenls #$Monster (#$SimClassFn "monster_demon_wraith")) *MappingMt*)
(sim-assert '(#$simGenls #$MissileLauncher (#$SimClassFn "weapon_rocketlauncher_mp")) *MappingMt*)
(sim-assert '(#$simGenls #$Microscope (#$SimClassFn "moveable_microscope")) *MappingMt*)
(sim-assert '(#$simGenls #$MedicalDevice (#$SimClassFn "moveable_item_medkit")) *MappingMt*)
(sim-assert '(#$simGenls #$MedicalDevice (#$SimClassFn "item_medkit_small_mp")) *MappingMt*)
(sim-assert '(#$simGenls #$MedicalDevice (#$SimClassFn "item_medkit_small")) *MappingMt*)
(sim-assert '(#$simGenls #$MedicalDevice (#$SimClassFn "item_medkit_mp")) *MappingMt*)
(sim-assert '(#$simGenls #$MedicalDevice (#$SimClassFn "item_medkit")) *MappingMt*)
(sim-assert '(#$simGenls #$MarinePersonnel (#$SimClassFn "player_simmarine_mp")) *MappingMt*)
(sim-assert '(#$simGenls #$MarinePersonnel (#$SimClassFn "model_mp_marine")) *MappingMt*)
(sim-assert '(#$simGenls #$MachineGun (#$SimClassFn "weapon_machinegun_mp")) *MappingMt*)
(sim-assert '(#$simGenls #$LiquidStorageTank (#$SimClassFn "moveable_explodingtank")) *MappingMt*)
(sim-assert '(#$simGenls #$LiquidStorageTank (#$SimClassFn "moveable_burningtank")) *MappingMt*)
(sim-assert '(#$simGenls #$LiquidStorageTank (#$SimClassFn "moveable_burningbarrel")) *MappingMt*)
(sim-assert '(#$simGenls #$LightingDevice (#$SimClassFn "light")) *MappingMt*)
(sim-assert '(#$simGenls #$LaptopComputer (#$SimClassFn "moveable_laptop")) *MappingMt*)
(sim-assert '(#$simGenls #$LaptopComputer (#$SimClassFn "moveable_blaptop")) *MappingMt*)
(sim-assert '(#$simGenls #$Lantern (#$SimClassFn "moveable_item_lantern_world")) *MappingMt*)
(sim-assert '(#$simGenls #$Jar (#$SimClassFn "moveable_plasticjar2")) *MappingMt*)
(sim-assert '(#$simGenls #$HypodermicSyringe (#$SimClassFn "moveable_infusion")) *MappingMt*)
(sim-assert '(#$simGenls #$Handgun (#$SimClassFn "weapon_pistol_mp")) *MappingMt*)
(sim-assert '(#$simGenls #$HandGrenade (#$SimClassFn "weapon_handgrenade_mp")) *MappingMt*)
(sim-assert '(#$simGenls #$Gunman (#$SimClassFn "human_marine_pistol")) *MappingMt*)
(sim-assert '(#$simGenls #$GliderChair (#$SimClassFn "moveable_tech_chair1")) *MappingMt*)
(sim-assert '(#$simGenls #$GeographicalSpaceRegion (#$SimClassFn "info_location")) *MappingMt*)
(sim-assert '(#$simGenls #$GeographicalPlace-0D (#$SimClassFn "info_player_start")) *MappingMt*)
(sim-assert '(#$simGenls #$GeographicalPlace-0D (#$SimClassFn "info_player_deathmatch")) *MappingMt*)
(sim-assert '(#$simGenls #$GarbageCan (#$SimClassFn "moveable_plasticbin")) *MappingMt*)
(sim-assert '(#$simGenls #$FloorLamp (#$SimClassFn "moveable_utilitylamp")) *MappingMt*)
(sim-assert '(#$simGenls #$FlatPanelDisplay (#$SimClassFn "moveable_monitorflip")) *MappingMt*)
(sim-assert '(#$simGenls #$FireExtinguisher (#$SimClassFn "moveable_fireext")) *MappingMt*)
(sim-assert '(#$simGenls #$ElectronicDevice (#$SimClassFn "moveable_gizmo1")) *MappingMt*)
(sim-assert '(#$simGenls #$EatingTable (#$SimClassFn "moveable_ktable")) *MappingMt*)
(sim-assert '(#$simGenls #$DrinkingMug (#$SimClassFn "moveable_foamcup")) *MappingMt*)
(sim-assert '(#$simGenls #$DrinkingGlass (#$SimClassFn "moveable_foamcup")) *MappingMt*)
(sim-assert '(#$simGenls #$Doorway (#$SimClassFn "func_door")) *MappingMt*)
(sim-assert '(#$simGenls #$DiningRoomChair (#$SimClassFn "moveable_kitchenchair")) *MappingMt*)
(sim-assert '(#$simGenls #$DiningRoomChair (#$SimClassFn "moveable_chair5")) *MappingMt*)
(sim-assert '(#$simGenls #$Diamond-Gem (#$SimClassFn "moveable_diamondbox")) *MappingMt*)
(sim-assert '(#$simGenls #$Diamond (#$SimClassFn "moveable_diamondbox_sm")) *MappingMt*)
(sim-assert '(#$simGenls #$DeskLamp (#$SimClassFn "moveable_desklamp")) *MappingMt*)
(sim-assert '(#$simGenls #$CustomModification (#$SimClassFn "moveable_gizmo3")) *MappingMt*)
(sim-assert '(#$simGenls #$Crate (#$SimClassFn "moveable_cartonbox6")) *MappingMt*)
(sim-assert '(#$simGenls #$Crate (#$SimClassFn "moveable_cartonbox5")) *MappingMt*)
(sim-assert '(#$simGenls #$Cooler-Container (#$SimClassFn "moveable_spigotcan")) *MappingMt*)
(sim-assert '(#$simGenls #$ContainerLid (#$SimClassFn "debris_barreltop2")) *MappingMt*)
(sim-assert '(#$simGenls #$ContainerLid (#$SimClassFn "debris_barreltop")) *MappingMt*)
(sim-assert '(#$simGenls #$Cone (#$SimClassFn "moveable_cone")) *MappingMt*)
(sim-assert '(#$simGenls #$ComputerWorkstation (#$SimClassFn "moveable_compcart")) *MappingMt*)
(sim-assert '(#$simGenls #$ComputerTerminal (#$SimClassFn "moveable_compcart")) *MappingMt*)
(sim-assert '(#$simGenls #$ComputerStand (#$SimClassFn "moveable_tablecart1")) *MappingMt*)
(sim-assert '(#$simGenls #$ComputerMonitor-Color (#$SimClassFn "moveable_monitor")) *MappingMt*)
(sim-assert '(#$simGenls #$ComputerMonitor-Color (#$SimClassFn "moveable_hangingmonitor")) *MappingMt*)
(sim-assert '(#$simGenls #$ComputerKeyboard (#$SimClassFn "moveable_keyboard1")) *MappingMt*)
(sim-assert '(#$simGenls #$ComputerHardwareItem (#$SimClassFn "moveable_gizmo2")) *MappingMt*)
(sim-assert '(#$simGenls #$Computer (#$SimClassFn "moveable_computer")) *MappingMt*)
(sim-assert '(#$simGenls #$Cheeseburger (#$SimClassFn "moveable_burger")) *MappingMt*)
(sim-assert '(#$simGenls #$Chair-PieceOfFurniture (#$SimClassFn "moveable_normchair")) *MappingMt*)
(sim-assert '(#$simGenls #$CellularTelephone (#$SimClassFn "moveable_phone")) *MappingMt*)
(sim-assert '(#$simGenls #$CardboardCanister (#$SimClassFn "moveable_cartonbox3")) *MappingMt*)
(sim-assert '(#$simGenls #$CardboardBox (#$SimClassFn "moveable_cartonbox2")) *MappingMt*)
(sim-assert '(#$simGenls #$Canister (#$SimClassFn "moveable_cannister")) *MappingMt*)
(sim-assert '(#$simGenls #$Can (#$SimClassFn "moveable_cokecan")) *MappingMt*)
(sim-assert '(#$simGenls #$Bucket (#$SimClassFn "moveable_mopbucket")) *MappingMt*)
(sim-assert '(#$simGenls #$Brick (#$SimClassFn "moveable_guardian_brick")) *MappingMt*)
(sim-assert '(#$simGenls #$Brick (#$SimClassFn "moveable_base_brick")) *MappingMt*)
(sim-assert '(#$simGenls #$BoxTheContainer (#$SimClassFn "moveable_cartonbox8")) *MappingMt*)
(sim-assert '(#$simGenls #$BoxTheContainer (#$SimClassFn "moveable_cartonbox7")) *MappingMt*)
(sim-assert '(#$simGenls #$BoxTheContainer (#$SimClassFn "moveable_cartonbox4")) *MappingMt*)
(sim-assert '(#$simGenls #$Boulder (#$SimClassFn "moveable_base_boulder")) *MappingMt*)
(sim-assert '(#$simGenls #$Bottle (#$SimClassFn "moveable_bottle1")) *MappingMt*)
(sim-assert '(#$simGenls #$Bottle (#$SimClassFn "bottle")) *MappingMt*)
(sim-assert '(#$simGenls #$BodyArmor (#$SimClassFn "item_armor_shard_mp")) *MappingMt*)
(sim-assert '(#$simGenls #$BarrelContainer (#$SimClassFn "moveable_explodingbarrel")) *MappingMt*)
(sim-assert '(#$simGenls #$BarrelContainer (#$SimClassFn "moveable_base_barrel")) *MappingMt*)
(sim-assert '(#$simGenls #$BarrelContainer (#$SimClassFn "moveable_barrel3")) *MappingMt*)
(sim-assert '(#$simGenls #$BarrelContainer (#$SimClassFn "moveable_barrel2")) *MappingMt*)
(sim-assert '(#$simGenls #$BarrelContainer (#$SimClassFn "moveable_barrel1")) *MappingMt*)
(sim-assert '(#$simGenls #$BPVLocation (#$SimClassFn "mud_room")) *MappingMt*)
(sim-assert '(#$simGenls #$BPVAgent (#$SimClassFn "mud_agent")) *MappingMt*)
(sim-assert '(#$simGenls #$ArmChair (#$SimClassFn "moveable_chair2")) *MappingMt*)
(sim-assert '(#$simGenls #$AmmunitionBelt (#$SimClassFn "ammo_clip_large_mp")) *MappingMt*)
(sim-assert '(#$simGenls #$AmmunitionBelt (#$SimClassFn "ammo_belt_small_mp")) *MappingMt*)
(sim-assert '(#$simGenls #$Agent-Generic (#$SimClassFn "mud_agent")) *MappingMt*)
|#

#|
(sim-assert '(#$simGenls #$Thing "zombie_default") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "wraith_rezeffect") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "worldrez") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_soulcube") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_shotgun_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_shotgun_double_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_shotgun_double") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_shotgun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_rocketlauncher_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_rocketlauncher") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_plasmagun_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_plasmagun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_pistol_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_pistol") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_pda") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_machinegun_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_machinegun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_handgrenade_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_handgrenade") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_grabber") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_flashlight") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_fists") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_chainsaw_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_chainsaw") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_chaingun_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_chaingun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_bloodstone_passive") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_bloodstone_active") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_bfg_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "weapon_bfg") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_zsecshotgun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_zombie_maint_bald") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_window_security") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_tunnel_imp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_security_helmet_mark") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_platform") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_maint_young_daniel") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_maint_old_charles") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_maint_bald_ross") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_maint_asian_eric") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_labcoat_young_todd") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_invasion_maintzombie") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_invasion_labzombie") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_invasion_labcoat") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_invasion_chestskull") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_invasion_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_impstairs") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_impintro_imp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_impintro_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_impcrawl_door") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_hallway_skulls") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_fatty") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_crazy_zombie") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_crazy_sci_cin") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_crazy_sci") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_cin_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "underground_cin_pistol") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "trigger_touch") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "trigger_timer") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "trigger_relay") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "trigger_once_entityname") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "trigger_once") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "trigger_multiple") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "trigger_hurt") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "trigger_flashlight") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "trigger_fade") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "trigger_facing") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "trigger_entityname") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "trigger_ctf_redflag") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "trigger_ctf_nodrop") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "trigger_ctf_flag_default") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "trigger_ctf_blueflag") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "trigger_count") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "text") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "teleport_shockwave") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "team_ctf_redflag_nugget") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "team_ctf_redflag") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "team_ctf_blueflag_nugget") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "team_ctf_blueflag") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_tip") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_show") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_setshadertime") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_setshaderparm") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_setmodel") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_setkeyval") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_setinfluence") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_setglobalparmtime") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_removeweapons") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_primaryobjective") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_null") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_npc_talk_triggered") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_npc_talk_secondary") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_npc_talk_primary") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_npc_talk") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_lock") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_light_fadeout") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_light_fadein") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_leveltrigger") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_givesecurity") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_giveemail") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_fadesoundclass") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_entity_setcolor") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_entity_fadeout") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_entity_fadein") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_endlevel") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_enableweapons") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_enablestamina") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_callobjectfunction") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "target_ai_followalternatepath") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "static_hunter") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "speaker") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "sound_powerup_berserk") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "sound_fragchamber_secretsteam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "sound_fragchamber_lights") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "sound_fragchamber_klaxon") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "sound_fragchamber_berserk") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "sound_ddm_wind") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "sound_ddm_generator") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "site_sci") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "site_hellknight") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "site_floor") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "shockwave_pound_attack") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "shockwave_electroblast") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "seeker_light") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ronn_cinematic_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "recycling_skybridge") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "recycling_revintro_rev") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "recycling_revintro_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "recycling_revintro_impact") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "recycling_revintro_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "recycling_mancintro_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "recycling_mancintro_pipes") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "recycling_mancintro_manc") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "recycling_mancintro_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "recycling_imp_cin") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "prop_wrench") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "prop_soft_desk_chair") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "prop_pistol") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "prop_metal_lounge_chair") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "prop_machinegun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "prop_lftflashlight_right") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "prop_lftflashlight") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "prop_foamcup") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "prop_dufflebag") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "prop_adrenaline") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_zombie_commando_cgun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_vulgar_fireball") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_soulblast") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_sentry_bullet") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_sabaoth_bfg") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_rocket_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_rocket") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_rev_rocket_cinematic") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_rev_rocket") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_powerball_guided") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_plasmatracer") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_plasmablast_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_plasmablast") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_mancubus_rocket") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_maledict_fireball") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_maledict_asteroid_huge") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_maledict_asteroid") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_impfireball") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_helltimefireball") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_helltime_killer") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_guardian_smash") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_grenade_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_grenade") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_fireball_invul_guided") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_fireball_invul") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_fireball_hellknight") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_fireball_berzerker") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_cyber_rocket") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_chaingunbullet_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_chaingunbullet") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_cacodemon_fireball") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_bullet_zsec_shotgun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_bullet_zsec_shield") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_bullet_zsec_pistol") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_bullet_zsec_machinegun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_bullet_turret") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_bullet_shotgun_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_bullet_shotgun_double") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_bullet_shotgun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_bullet_pistol_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_bullet_pistol") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_bullet_machinegun_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_bullet_machinegun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_bullet_char_soldier_machinegun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_bfg_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_bfg_cinematic") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "projectile_bfg") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "precacheextras") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "powerup_megahealth") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "powerup_invulnerability") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "powerup_invisibility") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "powerup_berserk") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "powerup_adrenaline") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "player_tshirt_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "player_suit_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "player_security_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "player_labcoat_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "player_jumpsuit_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "player_id_base") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "player_greenarmor_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "player_simmarine_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "player_simmarine_ctf") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "player_simmarine") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "player_base_dxp_sp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "player_base_dxp_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "player_base") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "phobos_tram") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "phobos_mcneil_typing") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "phobos_mcneil_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "phobos_mcneil_fem") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "phobos_mcneil_chair_cine") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "phobos_mcneil_chair") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "phobos_mcneil_camera") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "phobos_mcneil_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "phobos_marine") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "phobos_hunteroutro") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "phobos_hunterintro") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "phobos_hellknight") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "phobos_cinematic_player_death") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "phobos_cinematic_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "phobos_cinematic_mcneil") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "phobos_cinematic_inv_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "phobos_cinematic_hunterdeath") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "phobos_ai_dummy") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "pda_underground_security_helmet_mark") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "pda_underground_maint_young_daniel") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "pda_marscity_soldier_young_pda") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "pda_marscity_soldier_black") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "pda_marscity_soldier_bald_pda") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "pda_marscity_security_goggles_pda") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "pda_marscity_maint_hallway") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "pda_marscity_labcoat_black") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "pda_marscity_char_maint_asian_pda") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "pda_erebus_ron_gibbons_pda") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "pda_alphalabs_labcoat_bald") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "pda_alphalabs_fragchamber_scientist") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_waitfortrigger") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_waitforheadanim") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_wait") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_turn") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_talk_triggered") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_talk_secondary") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_talk_primary") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_talk") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_sentry_unlock_door") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_sentry_shutdown") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_sentry_light_on") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_sentry_light_off") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_sentry_lead_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_sentry_ignore_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_lookat") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_jump") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_hide") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_headanim") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_default") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_cycleanim") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_corner") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_conversation_listen") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_conversation") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_attack") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "path_anim") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "npcgroup_cinematic_scientist") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "npcgroup_cinematic_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "npc_harvest_test") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "npc_base") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_wrench") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_tablecart") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_spigotcan") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_shovel") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_rubble_") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_powerup_adrenaline") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_player_helmet") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_plasticjar") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_plasticbinmini") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_plasticbin") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_pickaxe") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_pc") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_paperwad") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_notakeg") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_mopbucket") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_mop") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_monitorflip") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_monitor") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_microscope") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_metalbox") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_marine_helmet") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_laptop") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_ktable") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_kitchenchair") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_keyboard") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_jackhammer") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_torso_pork") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_skull_pork") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_skelgib_skull") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_skelgib_rib") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_skelgib_bone") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_shotgun_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_shotgun_double") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_shotgun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_shield") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_rup_leg_pork") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_rup_arm_pork") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_rocketlauncher_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_rocketlauncher") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_plasmagun_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_plasmagun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_pistol_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_pistol") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_pelvis_pork") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_pda") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_medkit_small") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_machinegun_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_machinegun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_lup_leg_pork") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_left_waist_pork") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_lantern_world") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_lantern") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_helmet") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_head_pork") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_grenades_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_grenades") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_flashlight") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_envirotank") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_default") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_chainsaw_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_chainsaw") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_chaingun_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_chaingun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_bfg_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_bfg") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_backpack") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_armor_shard") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_armor_security") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_item_aircannister") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_infusion") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_hangingmonitor") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_guardian_brick") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_gizmo") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_foamcup") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_fireext") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_filecabinet") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_explodingtank") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_explodingbarrel") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_dxp_lamp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_dxp_bench") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_diamondbox_sm") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_diamondbox") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_desklamp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_deadsentry_leg") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_deadsentry_head") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_deadsentry_body") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_cone") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_computer") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_compcart") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_cokecan") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_chair") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_cartonbox") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_cannister") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_burningtank") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_burningbarrel") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_burgerboxopen") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_burgerboxclose") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_burger") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_bottle") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_bot_tech_bag") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_blaptop") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_berserkerbox") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_base_fixed") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_base_brick") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_base_barrel") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_base") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_barrelvb") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_barrelv") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_barrel_lid") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_barrel") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_ancient_staff") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_ancient_buzz_blade") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_ammo_shells_small") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_ammo_shells_large") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_ammo_rockets_small") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_ammo_rockets_large") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_ammo_grenade_small") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_ammo_clip_small") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_ammo_clip_large") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_ammo_cells_small") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_ammo_cells_large") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_ammo_bullets_small") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_ammo_bullets_large") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_ammo_bfg_small") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "moveable_ammo_belt_small") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zsec_shotgun_xray") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zsec_shotgun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zsec_shield") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zsec_pistol_slowfire") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zsec_pistol") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zsec_machinegun_xray") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zsec_machinegun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_zfem") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_tshirt_blown") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_tshirt_bald") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_suit_skinny") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_suit_neckstump") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_suit_bloodymouth") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_sawyer") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_morgue") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_maint_xray") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_maint_wrench") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_maint_skinny") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_maint_nojaw") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_maint_no_jaw") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_maint_flashlight") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_maint_fast_xray") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_maint_fast") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_maint_bald") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_maint") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_labcoat_skinny") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_labcoat_pipe") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_labcoat_neckstump") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_labcoat_limb") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_jumpsuit") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_hellgrowth") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_hazmat") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_fat_xray") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_fat_wrench") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_fat") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_commando_cgun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_commando") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_boney") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_bernie") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_zombie_base") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_turret_light") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_turret_ancient") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_turret") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_hunter_invul") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_hunter_helltime") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_hunter_berzerk") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_flying_lostsoul") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_flying_forgotten") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_flying_cacodemon") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_dummy_target") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_demon_wraith_dxp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_demon_trite_jump") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_demon_trite") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_demon_tick") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_demon_revenant") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_demon_pinky") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_demon_mancubus") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_demon_maggot") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_demon_imp_dxp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_demon_imp_crawler") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_demon_imp_crawl_armdoor") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_demon_imp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_demon_hellknight") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_demon_cherub") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_demon_archvile") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_default") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_boss_vagary") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_boss_sabaoth") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_boss_maledict_cinematic") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_boss_guardian_rezer") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_boss_guardian_seeker") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_boss_guardian") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_boss_dxp_maledict") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monster_boss_cyberdemon") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monorail_rider") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monorail_raisecommando_zct") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monorail_raisecommando_marine") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monorail_raisecommando_changer") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monorail_raisecommando_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monorail_raisecommando_betruger") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monorail_crashsight_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monorail_crash_debris") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "monorail_crash_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_zombie_zfem_right") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_zombie_zfem_left") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_zombie_maint_right") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_zombie_maint_push") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_zombie_maint_pull") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_zombie_maint_left") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_zombie_fat_right_wrench") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_zombie_fat_right") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_zombie_fat_left") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_zombie_chainsaw") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_wraithrightclaw") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_wraithleftclaw") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_wraithcenterattack") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_vulgarrightclaw") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_vulgarleftclaw") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_vulgarleapattack") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_vagrightclaw") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_vagleftclaw") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_triteleapattack") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_tritebite") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_sabaoth") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_revrightclaw") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_revleftclaw") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_revenant_right_down") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_revenant_right") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_revenant_left_down") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_revenant_left") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_revenant") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_pinky_right") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_pinky_left") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_pinky_back") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_mancubus_stomp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_magrightclaw") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_magleftclaw") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_maggotleapattack") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_lostsoul_charge") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_lostsoul") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_imprightclaw") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_impleftclaw") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_impleapattack") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_hunter_berserk_chargeattack") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_hunter_berserk_attack_right") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_hunter_berserk_attack_left") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_hunter_berserk_attack") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_helltime") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_hellknightrightclaw") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_hellknightleftclaw") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_hellknight_bite") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_forgotten_charge") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_forgotten") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_cyberdemon_kick") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_commandotentacle") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_commando_right") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_commando_push") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_commando_left") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_cherubrightclaw") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_cherubleftclaw") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_cherubleapattack") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_cacodemon") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_archrightclaw") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "melee_archleftclaw") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_zombie_morgue") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_walking_swann") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_suit_young_chair") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_suit_young") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_suit_asian_chair") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_soldier_young_pda") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_soldier_black") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_soldier_bald_pda") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_smallpda") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_ship") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_security_machinegun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_security_goggles_pda") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_sec_window") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_sec_checkin") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_receptionist_start") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_receptionist") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_reception_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_meeting_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_meeting_betruger") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_marine_helmet_p_walking") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_marine_helmet_p_directions_ver") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_maint_hallway") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_maint_ceiling") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_labcoat_young") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_labcoat_monitor") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_labcoat_black") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_imp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_hangar_swann") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_hangar_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_hangar_campbell") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_hangar") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_hallway_zombie") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_hallway_marine") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_dufflebag") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_civilian_kitchen") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_civilian_hallway") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_civilian_bathroom") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_civilian") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_cinematic_victim") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_cinematic_swann") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_cinematic_security") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_cinematic_sarge") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_cinematic_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_cinematic_doctor") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_cinematic_campbell") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_cinematic_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_cinematic_betruger") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_cin_imp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_cin_fatty") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "marscity_char_maint_asian_pda") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "maledict_smoke") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "maledict_rocket_cinematic_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "maledict_intro_cinematic_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "maledict_intro_cinematic") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "maledict_heart_flyin_cinematic_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "maledict_heart_closeup_cinematic_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "maledict_heart_cinematic_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "maledict_flamewall_sound") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "maledict_flamewall_base") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "maledict_flamewall") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "maledict_death_skull_smoke") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "maledict_death_skull") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "maledict_death_player_turnrun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "maledict_death_player_run") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "maledict_death_player_rocket") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "maledict_death_player_heart_flyin") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "maledict_death_player_heart") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "maledict_death_cinematic_split") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "maledict_death_cinematic_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "maledict_death_cinematic") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "light") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "item_videocd") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "item_team_default") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "item_powercell") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "item_pda") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "item_objectivecomplete") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "item_objective") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "item_keycard_generic") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "item_keycard_aco") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "item_key_yellow") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "item_key_blue") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "item_grabbercd") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "item_generic") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "item_generatorcd") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "item_envirotank") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "item_envirosuit") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "item_deploycd") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "item_default") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "item_backpack") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "item_artifact_tablet") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "item_armor_shard_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "item_armor_shard") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "item_armor_security_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "item_armor_security") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "item_aircannister") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "invul_electricwall") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "intro_scientist") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "info_vacuumseparator") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "info_vacuum") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "info_portalsky") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "info_player_teleport") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "info_player_start") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "info_player_deathmatch") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "info_locationseparator") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "info_location") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "hunter_invulnerability_pound_attack") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "hunter_invulnerability_electroblast_attack") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "hunter_invul_melee_normal") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "hunter_invul_melee_inv") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "hellhole_walkmarine") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "hellhole_soulcube") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "hellhole_maggot") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "hellhole_imp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "hellhole_cyberdeathrocks") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "hellhole_cyberbricks") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "hellhole_coffinbricks") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "hellhole_cin_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "hellhole_cin_cyberdemon") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "hellhole_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "hell_soulcube") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "hell_rstep") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "hell_player_maledict_intro") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "hell_maledict_intro_cinematic") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "hell_lstep") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "hell_intro_cinematic_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "hell_guardianintro_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "hell_guardiandeath_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "hell_cin_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "hell_cin_guardian") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "harvest_shockwave") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "guardian_poundground_explosion") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_teleporter") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_static") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_splinemover") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_splat") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_smoke") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_shockwave") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_shaking") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_securitycamera") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_rotating") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_riser") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_remove") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_radiochatter") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_portal") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_plat") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_phantom_objects") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_pendulum") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_mover_amodel") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_mover") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_mountedobject") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_itemremove") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_group") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_fx") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_fracture") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_forcefield") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_explosion") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_emitter") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_elevator") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_earthquake") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_damage") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_damagable") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_clipmodel") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_cameraview") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_cameratarget") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_bobbing") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_beam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_animate") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_activator") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_aas_portal") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "func_aas_obstacle") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_wall_explode") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_vulgarintro_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_trapped_npc") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_transform_npc") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_technician") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_spooked") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_scientist_cloud") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_scientist") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_rocks_p") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_mcneil_fem") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_mcneil_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_intro_scientist") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_intro_rocks") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_intro_plyr_helmet") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_intro_plyr_heart_p") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_intro_plyr_heart") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_intro_pda_for_roq") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_intro_pda") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_intro_mcneil") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_intro_marine_for_roq") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_intro_marine") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_intro_flash_for_roq") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_intro_flash") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_intro_detonate") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_intro_camera_p") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_intro_camera") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_hunterintro_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_ggun_marine_die") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_fan_npc") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_envirosuit_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_envirosuit_on_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_envirosuit_off_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_dying_marine") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_cloud_player_cinematic") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_cloud_cinematic_camera_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_cloud_cinematic") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_cinematic_wall") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_cinematic_player_vulgarintro") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_cinematic_player_start") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_cinematic_player_fill") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_cinematic_player_end") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_cinematic_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_cinematic_marine_gravitygun_end") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_cinematic_marine_gravitygun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_cinematic_imp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_cinematic_hunterdeath") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_cinematic_hunter") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_cinematic_camera") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_cinematic_cam_death") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_cinematic_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_bottech") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "erebus_ai_dummy") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "envirosuit_light") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_xianbutton_") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_swinglight_sl_fixed") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_swinglight_sl") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_swinglight_round") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_swinglight_long_wbulbs_fixed") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_swinglight_long_wbulbs_") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_swinglight_long_wbulbs") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_storagecabinet_openback") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_storagecabinet") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_zsecs") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_zsecp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_zsecm_xray") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_zsecm") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_zscientist") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_zmaint") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_suit") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_skeleton") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_sentry") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_revenant") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_poppy") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_pinky") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_mummy_fixed") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_mummy") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_marine_stump_gib") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_marine_stump") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_marine_helmet") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_marine") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_maint") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_lab") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_imp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_hellknight") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_hazmat") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_gibbable_base") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_fatty") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_commando") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_cherub") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_boney") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ragdoll_archvile") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_phobos_bridge") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_pcellgen_single") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_pcellgen") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_lostsoul_fx") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_lamp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_inviso") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_hellpuzzle_smoke") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_hellchain") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_harvest_marine") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_harvest_maint") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_harvest_lab") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_harvest_hazmat") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_harvest_default") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_harvest") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_grenade_explodeinhand") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_gibs_torso") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_gibs_spine") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_gibs_ruparmstub") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_gibs_rtuplegstump") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_gibs_rtlolegstump") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_gibs_leftleg") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_gibs_leftarm") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_evilmeat") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_dxp_vehicle") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_craneplatform_caverns") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_crane") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_cage_corpse") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_bfgcase") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_bfg_overcharge") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_airlockdoor") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "env_ai_dummy") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "enpro_wounded_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "enpro_wounded_marine") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "enpro_wounded_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "enpro_tuberide_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "enpro_swann") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "enpro_soldier") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "enpro_monitor_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "enpro_lostintro_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "enpro_lost_spine") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "enpro_lost_chair") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "enpro_keycard") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "enpro_glass_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "enpro_exit_imp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "enpro_exit_helmet") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "enpro_exit_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "enpro_escape_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "enpro_cin_wraith") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "enpro_cin_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "enpro_cin_machinegun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "enpro_cin_lostsoul") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "enpro_cin_female") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "enpro_campbell") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ending_debris") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ending_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "deltax_railing_pinkyattack") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "deltax_doorframe_pinkyattack") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "deltab_waterwires") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "deltab_tankrevwires") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "deltab_tankrev") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "deltab_tankimpwires") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "deltab_tankimp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "deltab_keycardzombie") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "deltab_imp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "deltab_hazmat") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "deltaa_zombie_office") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "deltaa_teleporter_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "deltaa_teleporter") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "deltaa_scientist_return") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "deltaa_scientist_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "deltaa_scientist") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "deltaa_sci_tele_hell_visions_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "deltaa_player_scientist") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "deltaa_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "deltaa_imp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "deltaa_fatty") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "deltaa_commando") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "deltaa_cin_fatty") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "delta_wounded_swann") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "delta_scipull") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "delta_hkintro_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "delta_hazguy") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "delta_cin_teleporter") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "delta_cin_player_start") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "delta_cin_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "delta_cin_hk") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "delta_cin_hazguy") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "delta_betruger") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "debris_shrapnel") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "debris_shotgunbrass") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "debris_largeshrapnel") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "debris_brass") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "debris_barreltop") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "debris_barrelpiece") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_zombie_commando_cgun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_vulgar_fireball_splash") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_vulgar_fireball_catch") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_vulgar_fireball") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_vagaryobject") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_trite_explode") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_triggerhurt_toxin_") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_triggerhurt_toxin") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_triggerhurt_") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_tick_explode") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_thrown_ragdoll") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_telefrag") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_suicide") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_soulsplash_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_soulsplash") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_soulblast_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_soulblast") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_softfall_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_softfall") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_smallexplosion") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_shotgun_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_shotgun_double") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_shotgun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_sentry_bullet") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_rocketsplash_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_rocketsplash") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_rocketdirect_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_rocketdirect") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_revrocketsplash") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_revrocket_catch") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_revrocket") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_plasmatracer") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_plasmablast_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_plasmablast") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_paintrigger") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_noair") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_movercrush") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_moveable_xlarge") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_moveable_tiny") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_moveable_small") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_moveable_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_moveable_medium") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_moveable_large") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_moveable_carton") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_mancubussplash") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_mancubusblast_catch") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_mancubusblast") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_maledictfirewall") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_maledict_fireball_splash") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_maledict_fireball_catch") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_maledict_fireball") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_maledict_asteroid_splash") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_maledict_asteroid_catch") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_maledict_asteroid") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_lightbreak") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_lava") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_killerfireball_splash") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_killer_fireball") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_invulnarea") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_invulelectricwall") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_impfireball_splash") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_impfireball_catch") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_impfireball") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_hugeexplosion") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_helltimefireball_splash") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_helltimefireball") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_hardfall_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_hardfall") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_guardianpoundground") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_guardianheadbutt") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_guardiancharge") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_guardian_smash_splash") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_guardian_smash_direct") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_grenadesplash_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_grenadesplash_inhand_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_grenadesplash_inhand") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_grenadesplash") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_grenadedirect_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_grenadedirect") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_gib") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_generic") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_fragchamberpipe") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_fragchamberfog") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_flashlight_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_flashlight") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_fists_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_fists") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_fireball_invul_splash") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_fireball_invul_catch") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_fireball_invul") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_fireball_hellknight_splash") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_fireball_hellknight_catch") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_fireball_hellknight") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_fireball_berzerker_splash") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_fireball_berzerker") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_fatalfall_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_fatalfall") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_explosion") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_explodingbarrel_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_explodingbarrel") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_cyberrocketsplash") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_cyberrocketdirect") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_crush") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_co_electric_trac") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_chainsaw_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_chainsaw") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_cacodemon_fireball_splash") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_cacodemon_fireball_catch") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_cacodemon_fireball") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_bullet_zsec_shotgun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_bullet_zsec_shield") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_bullet_zsec_pistol") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_bullet_zsec_machinegun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_bullet_turret") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_bullet_pistol_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_bullet_pistol") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_bullet_machinegun_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_bullet_machinegun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_bullet_char_soldier_machinegun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_bullet_chaingun_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_bullet_chaingun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_bfgsplash_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_bfgsplash_cinematic") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_bfgsplash") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_bfgfreq_cinematic") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_bfgfreq") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_bfg_overcharge") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_bfg_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_bfg_cinematic") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_bfg") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_archvileincinerate") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "damage_archvilefirewall") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "mud_agent") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "cpuboss_cin_sabaoth") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "cpuboss_cin_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "cpuboss_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "cpu_wounded_campbell") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "cpu_monster_trite") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "cpu_camphunt_campbell") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "cpu_camphunt_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "commoutside_vehicle") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "commoutside_swann") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "commoutside_hellgoo") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "commoutside_fatty") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "commoutside_campbell") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "comm_sentryloader") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "comm_sentry_blank") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "comm_sentry") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "cloud") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "cin_base") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "character_soldier_machinegun") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "character_default") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "char_swann") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "char_sentry_flashlight") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "char_sentry") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "char_security_goggles_pistol") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "char_mcneil_fem") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "char_marine_young_chair") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "char_marine_asian_chair") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "char_labcoat_black") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "char_hazmat") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "char_campbell_bfgcase") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "char_campbell_bfg") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "char_campbell") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "char_betruger") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "caverns_vagrocks") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "caverns_vagary") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "caverns_imp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "caverns_hkwall") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "caverns_hellknight") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "caverns_char_maint_scared") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "caverns_bridgefront") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "caverns_bridgeback") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "caverns_bridge") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "caverns_boulderbridge") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "bot_sabot") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "bot_base") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "archvile_incinerate") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "archvile_flamewall") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_types_dxp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_types") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_shells_small_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_shells_small") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_shells_large_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_shells_large") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_rockets_small_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_rockets_small") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_rockets_large_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_rockets_large") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_names") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_grenade_small_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_grenade_small") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_clip_small_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_clip_small") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_clip_large_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_clip_large") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_cells_small_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_cells_small") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_cells_large_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_cells_large") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_bullets_small_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_bullets_small") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_bullets_large_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_bullets_large") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_bloodstone_small") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_bloodstone_normal") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_bfg_small") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_belt_small_mp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ammo_belt_small") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "alphalabs_vagaryintro_vagary") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "alphalabs_vagaryintro_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "alphalabs_vagaryintro_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "alphalabs_scientist") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "alphalabs_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "alphalabs_marine_helmet_p") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "alphalabs_maggot") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "alphalabs_labcoat_bald") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "alphalabs_imp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "alphalabs_helldoll") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "alphalabs_fragchamber_skeleton") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "alphalabs_fragchamber_scientist") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "alphalabs_elevatorenv") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "alphalabs_cin_imp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "alphalabs_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ai_lostcombat") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ai_attackcone_turret") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ai_attackcone_once") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "ai_attackcone") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "admin_wounded_marine") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "admin_vent_swann") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "admin_railing_pinkyattack") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "admin_pinkyattack_pinky") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "admin_pinkyattack_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "admin_pinky_glassbreak") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "admin_overhear_swann") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "admin_overhear_player") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "admin_overhear_campbell_bfg") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "admin_overhear_campbell") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "admin_overhear_cam") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "admin_impdoors") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "admin_imp") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "admin_doorframe_pinkyattack") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "admin_bfgcase") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "aas_sabaoth") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "aas_mancubus") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "aas_guardian") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "aas_cyberdemon") *MappingMt*)
(sim-assert '(#$simGenls #$Thing "aas") *MappingMt*)

|#

(csetq *DEFINITIONMT* *WorldVocabularyMt*)
(csetq *VOCABMT* *WorldVocabularyMt*)


;;(templateTopicGenls SimFoundInLocations-TopicType SimTypeCreatable)
(find-or-create-constant "SimWorldRez-TopicType")
(cyc-assert '(#$isa #$SimWorldRez-TopicType #$FormulaTemplateTopicType) *UVMT* '(:DIRECTION :FORWARD))
(cyc-assert '(#$titleForFormulaTemplateType-String #$SimWorldRez-TopicType "Topic template for constraining worldrezing") *DEFINITIONMT* '(:DIRECTION :FORWARD))
(cyc-assert '(#$formulaTemplateTypeHasTopicType #$IndividualTerrainArtifactTemplate #$SimWorldRez-TopicType) *VOCABMT* '(:DIRECTION :FORWARD))
;;(cyc-assert '(#$templateTopicGenls #$SimWorldRez-TopicType #$SimItem) *VOCABMT* '(:DIRECTION :FORWARD))

(find-or-create-constant "SimFoundInLocations-TopicType")
(cyc-assert '(#$isa #$SimFoundInLocations-TopicType #$FormulaTemplateTopicType) *UVMT* '(:DIRECTION :FORWARD))
;;(cyc-assert '(#$isa #$SimFoundInLocations-TopicType #$Collection) *DEFINITIONMT* '(:DIRECTION :FORWARD))
(cyc-assert '(#$simGenls #$SimFoundInLocations-TopicType #$SimWorldRez-TopicType) *DEFINITIONMT* '(:DIRECTION :FORWARD))
(cyc-assert '(#$titleForFormulaTemplateType-String #$SimFoundInLocations-TopicType "Topic template for constraining objects in rooms") *DEFINITIONMT* '(:DIRECTION :FORWARD))
;;(cyc-assert '(#$formulaTemplateTypeHasTopicType #$IndividualTerrainArtifactTemplate #$SimFoundInLocations-TopicType) *VOCABMT* '(:DIRECTION :FORWARD))
;;(cyc-assert '(#$templateTopicGenls #$SimFoundInLocations-TopicType #$SimTypeCreatable) *VOCABMT* '(:DIRECTION :FORWARD))


(sim-assert '(#$disjointWith #$Sim-ProvableSentence #$Sim-UnprovableSentence) *VOCABMT* '(:DIRECTION :FORWARD))

;;;;  
;;(sim-assert '(#$implies (#$and (#$isa ?I ?TYPE)(#$simGenls ?TYPE #$SimItem)(#$termStrings ?I ?S) (#$sim:denotesContextually ?S (#$RezTopicTypeFn ?TYPE) ?C)) (#$ist *MAPPINGMT* (#$isa ?I ?C))) *MAPPINGMT* '(:DIRECTION :FORWARD))

#+SimItem	
'(sim-assert 
 '(#$implies 
    (#$and  (#$isa ?I #$SimItem)
	 (#$webSearchableStrings ?I ?S) 
	 (#$sim:denotesContextually ?S ?C)
	 (#$simGenls ?C #$SomethingExisting)  
	 (#$simGenls ?C #$SpatialThing)  
	 (#$simGenls ?C #$ThreeDimensionalThing)  
	 (#$simRezClass ?I ?C ?P)) 
    (#$ist *MAPPINGMT* (?P ?I ?C))) *MAPPINGMT* '(:DIRECTION :FORWARD))


(sim-assert '(#$termStrings #$Brig "brig") *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$termStrings #$Bedroom "quarters") *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$termStrings #$MachinePistol "phaser") *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$termStrings #$AirplaneCockpit "bridge") *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$termStrings #$PropositionalConceptualWork "world") *MAPPINGMT*)
(sim-assert '(#$termStrings #$Bottle "bottle") *MAPPINGMT*)
(sim-assert '(#$termStrings #$MarinePersonnel "ensign") *MAPPINGMT*)
(sim-assert '(#$termStrings #$Leader "Commander") *MAPPINGMT*)
(sim-assert '(#$termStrings #$SignTheDisplay "sign") *MAPPINGMT*)
(sim-assert '(#$termStrings #$Elevator "Turbolift") *MAPPINGMT*)
(sim-assert '(#$termStrings #$Bottle "bottle") *MAPPINGMT*)
(sim-assert '(#$termStrings #$LightingDevice "light") *MAPPINGMT*)

(find-or-create-constant "RezTopicTypeFn")
(sim-assert '(#$isa #$RezTopicTypeFn #$UnaryFunction) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$isa #$RezTopicTypeFn #$ReifiableFunction) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$skolemizeForward #$RezTopicTypeFn) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$resultIsa #$RezTopicTypeFn #$FormulaTemplateTopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$arg1Genl #$RezTopicTypeFn #$SimItem) *MAPPINGMT* '(:DIRECTION :FORWARD))

(sim-assert '(#$isa (#$RezTopicTypeFn #$BPVAgent) #$FormulaTemplateTopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$isa (#$RezTopicTypeFn #$BPVArtifact) #$FormulaTemplateTopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$isa (#$RezTopicTypeFn #$BPVLocation) #$FormulaTemplateTopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))

(sim-assert '(#$simGenls (#$RezTopicTypeFn #$BPVAgent) #$SimWorldRez-TopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$simGenls (#$RezTopicTypeFn #$BPVArtifact) #$SimWorldRez-TopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$simGenls (#$RezTopicTypeFn #$BPVLocation) #$SimWorldRez-TopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$implies (#$isa (#$RezTopicTypeFn ?COL) #$FormulaTemplateTopicType)(#$templateTopicGenls (#$RezTopicTypeFn ?COL) ?COL)) *MAPPINGMT* '(:DIRECTION :FORWARD))
;;;;;;(sim-assert '(#$implies (#$and (#$isa ?I ?TYPE)(#$simGenls ?TYPE #$SimItem)(#$termStrings ?I ?S) (#$sim:denotesContextually ?S (#$RezTopicTypeFn ?TYPE) ?C)) (#$ist *MAPPINGMT* (#$isa ?I ?C))) *MAPPINGMT* '(:DIRECTION :FORWARD))
  

;;;;(sim-assert '(#$implies (#$isa (#$simViolatesSomeConstraint ?C) #$Sim-UnprovableSentence) (#$simExceedsSomeConstraint ?C)) *MAPPINGMT*)
  

;;;;(sim-assert '(#$locatedAtPoint-Spatial #$Area1000 (#$Point3Fn 0 0 0)) *MAPPINGMT* '(:DIRECTION :FORWARD))

#|

(find-or-create-constant "sim:parsesContextually")
(sim-assert '(#$isa #$sim:parsesContextually #$Predicate) *UVMT*)
(sim-assert '(#$comment #$sim:parsesContextually "HL Predicate (#$parsesContextually ?String ?Ctx ?Result) uses the (parse-a-sentence-completely ?String #$AllEnglishLexicalMicrotheoryPSC) to produce ?Result that is choosen best in ?Ctx.") *UVMT*)
(sim-assert '(#$isa #$sim:parsesContextually #$RemovalModuleSupportedPredicate-Specific) #$CycAPIMt)
(sim-assert '(#$arity #$sim:parsesContextually 3) *UVMT*)
(sim-assert '(#$arg1Isa #$sim:parsesContextually #$TextString) *UVMT*)
(sim-assert '(#$arg2Isa #$sim:parsesContextually #$Thing) *UVMT*)
(sim-assert '(#$arg3Isa #$sim:parsesContextually #$Thing) *UVMT*)
(inference-removal-module :removal-parsesContextually-bound-bound-unbound
 '(:sense :pos 
	:predicate #$sim:parsesContextually 
	:required-pattern (#$sim:parsesContextually :fully-bound :fully-bound :not-fully-bound) 
	:cost-expression 0 :completeness :complete 
	:input-extract-pattern (:template (#$sim:parsesContextually (:bind value-1) (:bind value-2) :anything) ((:value value-1) (:value value-2)))
	:input-verify-pattern :anything
	:output-generate-pattern (:call parsesContextually :input)
	:output-construct-pattern (#$sim:parsesContextually (:value value-1) (:value value-2) :input)))
(register-solely-specific-removal-module-predicate #$sim:parsesContextually)
(define parsesContextually (vs) 
 (clet ((str (car vs))(mt (nth 1 vs))(res nil))
 (cand (stringp str) 
 (progn
 (cand 
  (cq1 (list #$isa mt #$Microtheory))
  (progn 
	 (csetq res (parse-a-question-completely str mt))
	 (cand res (ret res))
	 (csetq res (parse-a-sentence-completely str mt))
	 (cand res (ret res))))
 (csetq res (parse-a-question-completely str #$AllEnglishLexicalMicrotheoryPSC))
 (cand res (ret res))
 (ret (parse-a-sentence-completely str #$AllEnglishLexicalMicrotheoryPSC))))))

(define parsesContextually (vs) 
 (clet ((str (car vs))(mt (nth 1 vs))(res nil))
 (cand (stringp str) 
 (csetq res (parse-a-question-completely str #$AllEnglishTemplateMt))
 (cand res (ret res))
 (csetq res (parse-a-sentence-completely str #$AllEnglishTemplateMt))
 (cand res (ret res))
 (csetq res (parse-a-question-completely str #$AllEnglishLexicalMicrotheoryPSC))
 (cand res (ret res))
 (ret (parse-a-sentence-completely str #$AllEnglishLexicalMicrotheoryPSC)))))

(define parsesContextually (vs) 
 (clet ((str (car vs))(mt (nth 1 vs))(res nil))
 (cand (stringp str) 
 (csetq res (parse-a-question-completely str #$AllEnglishLexicalMicrotheoryPSC))
 (cand res (ret res))
 (ret (parse-a-sentence-completely str #$AllEnglishLexicalMicrotheoryPSC)))))
 
;;(sim-assert '(#$implies (#$and(#$definiteDescriptions ?AREA ?TEXT)(#$sim:parsesContextually ?TEXT #$SimWorldRez-TopicType ?CYCL))(#$futureAssertion ?AREA ?TEXT ?CYCL))*MAPPINGMT* '(:DIRECTION :FORWARD))


 
;;;;(parsesContextually1 "What is the population of the Turkey?")
;;;;(parsesContextually1 "How many people live in the USA?")

(define parsesContextually1 (str) (parsesContextually (list str '#$SimWorldRez-TopicType)))

Three large chairs in the northern part of the room, in front of the railing, face the screen

Directly in front of you is a thick railing that contains many different computer panels used for the tactical systems of the ship

You can't see a bed in this room, but you figure it's because Data doesn't sleep 
)))


(cyc-query '(#$sim:parsesContextually "I love you" #$SimWorldRez-TopicType ?X) *UVMT*) 

;;(define bestdenoted (l ctx) (cand l (clet ((best (car l))) (progn (cdolist (x (cdr l)) (csetq best (better1 best x ctx))) (ret (list (cdr best)))))))


;;sim:denotesContextually
(find-or-create-constant "sim:denotesContextually")
(sim-assert '(#$isa #$sim:denotesContextually #$Predicate) *UVMT*)
(sim-assert '(#$comment #$sim:denotesContextually "HL Predicate (#$denotesContextually ?String ?Ctx ?Result) uses the (denotation-mapper ?String) to produce ?Result that is choosen best in ?Ctx.") *UVMT*)
(sim-assert '(#$isa #$sim:denotesContextually #$RemovalModuleSupportedPredicate-Specific) #$CycAPIMt)
(sim-assert '(#$arity #$sim:denotesContextually 3) *UVMT*)
(sim-assert '(#$arg1Isa #$sim:denotesContextually #$TextString) *UVMT*)
(sim-assert '(#$arg2Isa #$sim:denotesContextually #$Thing) *UVMT*)
(sim-assert '(#$arg3Isa #$sim:denotesContextually #$Thing) *UVMT*)
;;;;(sim-assert '(#$genlPreds #$meetsSpecification #$sim:denotesContextually) *UVMT*)
(sim-assert '(#$conceptuallyRelated #$sim:denotesContextually #$termRankingPreference) *UVMT*)
(sim-assert '(#$conceptuallyRelated #$sim:denotesContextually #$Criterion) *UVMT*)
(sim-assert '(#$conceptuallyRelated #$sim:denotesContextually #$superiorTypeWRTCriterion) *UVMT*)
(sim-assert '(#$conceptuallyRelated #$sim:denotesContextually #$superiorWRTCriterion) *UVMT*)
(sim-assert '(#$conceptuallyRelated #$sim:denotesContextually #$ratingTypeCriterion) *UVMT*)
(sim-assert '(#$conceptuallyRelated #$sim:denotesContextually #$meetsSpecification) *UVMT*)
(sim-assert '(#$conceptuallyRelated #$sim:denotesContextually #$simExceedsSomeConstraint) *UVMT*)
(sim-assert '(#$conceptuallyRelated #$sim:denotesContextually #$simViolatesSomeConstraint) *UVMT*)


;;;;(sim-assert (#$isa #$SimWorldRez-TopicType #$FormulaTemplateTopicType
(inference-removal-module :removal-denotesContextually-bound-bound-unbound
 '(:sense :pos 
	:predicate #$sim:denotesContextually 
	:required-pattern (#$sim:denotesContextually :fully-bound :fully-bound :not-fully-bound) 
	:cost-expression 0 :completeness :complete 
	:input-extract-pattern (:template (#$sim:denotesContextually (:bind value-1) (:bind value-2) :anything) ((:value value-1) (:value value-2)))
	:input-verify-pattern :anything
	:output-generate-pattern (:call denoter :input)
	:output-construct-pattern (#$sim:denotesContextually (:value value-1) (:value value-2) :input)))
(register-solely-specific-removal-module-predicate #$sim:denotesContextually)
(define denoter (vs) (cand (stringp (car vs))(constantp (nth 1 vs)) (ret (bestdenoted (remove-violaters (denotation-mapper (car vs) nil :diligent) (nth 1 vs)) (nth 1 vs)))))
(define bestdenoted (l ctx) (cand l (clet ((best (car l))) (progn (cdolist (x (cdr l)) (csetq best (better1 best x ctx))) (ret (list (cdr best)))))))


(find-or-create-constant "sim:denotationMapper")
(sim-assert '(#$isa #$sim:denotationMapper #$Predicate) *UVMT*)
(sim-assert '(#$comment #$sim:denotationMapper "HL Predicate (#$denotationMapper ?String ?Result ?Lex) uses the (denotation-mapper ?String) to produce ?Result that is choosen best in ?Ctx.") *UVMT*)
(sim-assert '(#$isa #$sim:denotationMapper #$RemovalModuleSupportedPredicate-Specific) #$CycAPIMt)
(sim-assert '(#$arity #$sim:denotationMapper 3) *UVMT*)
(sim-assert '(#$arg1Isa #$sim:denotationMapper #$TextString) *UVMT*)
(sim-assert '(#$arg2Isa #$sim:denotationMapper #$Thing) *UVMT*)
(sim-assert '(#$arg3Isa #$sim:denotationMapper #$Thing) *UVMT*)
(sim-assert '(#$isa #$SimWorldRez-TopicType #$FormulaTemplateTopicType) *UVMT*)
(inference-removal-module :removal-denotationMapper-bound-unbound-unbound
 '(:sense :pos 
	:predicate #$sim:denotationMapper 
	:required-pattern (#$sim:denotationMapper :fully-bound :not-fully-bound :not-fully-bound) 
	:cost-expression 0 :completeness :complete 
	:input-extract-pattern (:template (#$sim:denotationMapper (:bind value-1) :anything :anything) ((:value value-1)))
	:input-verify-pattern :anything
	:output-generate-pattern (:call dentoterMapper :input)))
(register-solely-specific-removal-module-predicate #$sim:denotationMapper)
(define dentoterMapper (calv) (ret (cand (stringp (car calv))
  (clet ((str (car calv))(lst (denotation-mapper str nil))(results nil))
  (cdolist (x lst) (csetq results (cons (list #$sim:denotationMapper str (car x) (cdr x)) results)))
  (ret results)))))


;;(cyc-query '(#$sim:denotationMapper "good" ?C ?Y) '#$EverythingPSC)

(define remove-violaters (lst ctx)
 (ret (cand (consp lst)
 (clet ((results nil))
 (cdolist (x lst) 
 (cand (consp x) (null (cq (list #$simViolatesSomeConstraint (cdr x) ctx))) (csetq results (cons x results))))
 (ret results)))))
          
;;;;(cyc-query '(#$and (#$isa ?I #$SimItem) (#$nameString ?I ?S) (#$sim:denotesContextually ?S #$SimWorldRez-TopicType ?C)) '#$EverythingPSC)
  
(define cq1 (q &optional (hmt #$EverythingPSC)) (ret (car (cyc-query q hmt '(:backchain T :time 15 :number 1)))))
(define cq (q &optional (hmt #$EverythingPSC)) (ret (cyc-query q hmt '(:backchain T :time 30))))
(define cq (q &optional (hmt #$EverythingPSC)) (clet ((result (cyc-query q hmt '(:backchain T)))) (print (list q hmt result)) (ret result)))
(define better1 (x y ctx) (clet ((best (better2 y x ctx))) (print (list (cdr x) (cdr y) best)) (ret best)))

(print "loading termStrings.")

 
;;;; (print (list x y))(force-output)
;;;; Assume violators are removed first with #'remove-violaters
(define better1 (x y ctx)
 (clet ((xx (cdr x)) (yy (cdr y)))
 (pcond 
 ((eq xx yy) (ret x))
 ((cq (list #$simExceedsSomeConstraint xx ctx)) (ret x))
 ((cq (list #$simExceedsSomeConstraint yy ctx)) (ret y))
 ((cq (list #$termRankingPreference ctx xx yy)) (ret xx))
 ((cq (list #$termRankingPreference ctx yy xx)) (ret yy))
 ((cq (list #$superiorWRTCriterion xx yy ctx)) (ret xx))
 ((cq (list #$superiorWRTCriterion yy xx ctx)) (ret yy))
 ((> (length (car x)) (length (car y))) (ret x))
 ((< (length (car x)) (length (car y))) (ret y))
 ((consp yy) (ret x))
 ((consp xx) (ret y))
 ( (> (length (all-term-assertions xx)) (length (all-term-assertions yy))) (ret y))
 ( (< (length (all-term-assertions xx)) (length (all-term-assertions yy))) (ret x))
 (T (ret x)))))
      

(define denoter1 (x) (ret (denoter (list x '#$SimWorldRez-TopicType))))
(define denoter2 (str) (ret (remove-violaters (denotation-mapper str nil :diligent) '#$SimWorldRez-TopicType)))

(cq '(#$simViolatesSomeConstraint #$Area1031 #$SimWorldRez-TopicType))
(load "/rcyc/cycsim/startrek.lisp")
(load "/rcyc/cycsim/mudtool.lisp")
(denoter1 "Quarters")
(denoter1 "room")
(denoter1 "bridge")
(denoter1 "trombone")
(denoter1 "cargo bay")
(denoter1 "captain")
 
 ((cq (list #$simGenls (cdr x) #$SimItem)) (ret y))
 ((cq (list #$simGenls (cdr y) #$SimItem)) (ret x))
 ((cq (list #$isa (cdr x) #$ChromaticColor)) (ret x))
 ((cq (list #$isa (cdr y) #$ChromaticColor)) (ret y))
 ((cq (list #$simGenls (cdr x) #$Device-SingleUser)) (ret x))
 ((cq (list #$simGenls (cdr y) #$Device-SingleUser)) (ret y))
 ((cq (list #$simGenls (cdr x) #$Facility-Generic)) (ret x))
 ((cq (list #$simGenls (cdr y) #$Facility-Generic)) (ret y))
 ((cq (list #$simGenls (cdr x) #$ScalarInterval)) (ret y))
 ((cq (list #$simGenls (cdr y) #$ScalarInterval)) (ret x))
 ((cq (list #$simGenls (cdr x) #$Ocean)) (ret y))
 ((cq (list #$simGenls (cdr y) #$Ocean)) (ret x))
 ((cq (list #$simGenls (cdr x) #$IntangibleIndividual)) (ret y))
 ((cq (list #$simGenls (cdr y) #$IntangibleIndividual)) (ret x)) 
 ((cq (list #$simGenls (cdr x) #$Intangible)) (ret y))
 ((cq (list #$simGenls (cdr y) #$Intangible)) (ret x)) 
 ((cq (list #$simGenls (cdr x) #$AminoAcid)) (ret y))
 ((cq (list #$simGenls (cdr y) #$AminoAcid)) (ret x))   
 ((cq (list #$simGenls (cdr x) #$MolecularComponent)) (ret y))
 ((cq (list #$simGenls (cdr y) #$MolecularComponent)) (ret x)) 
 ((cq (list #$simGenls (cdr x) #$AspatialInformationStore)) (ret y))
 ((cq (list #$simGenls (cdr y) #$AspatialInformationStore)) (ret x)) 
 ((cq (list #$simGenls (cdr x) #$Event)) (ret y))
 ((cq (list #$simGenls (cdr y) #$Event)) (ret x))
 ((cq (list #$isa (cdr x) #$Individual)) (ret y))
 ((cq (list #$isa (cdr y) #$Individual)) (ret x))
 ((> (length (car x)) (length (car y))) (ret x))
 ((null (cq (list #$isa (cdr x) #$Collection))) (ret y))
 ((null (cq (list #$isa (cdr y) #$Collection))) (ret x))


(sim-assert '
 (#$equals 
 (#$definiteDescriptions ?PLACE "Two large windows offer a great view of space") 
 (#$thereExistExactly 2 ?INST 
 (#$and 
  (#$isa ?INST #$VisualInformationBearingThing) 
  (#$isa ?INST #$WindowPortal) 
  (#$objectFoundInLocation ?INST ?PLACE) 
  (#$visuallyDepicts ?INST #$OuterSpace))))	*MAPPINGMT*)
  

denotation-mapper

(#$implies 
 (#$and 
 (#$isa ?I #$SimItem) 
 (#$or (#$termStrings ?I ?S) 
 (#$webSearchableStrings ?I ?S) 
 (#$nameString ?I ?S)) 
 (#$sim:denotesContextually ?S ?C) 
 (#$simRezClass ?I ?C ?P)) 
 (#$ist *MAPPINGMT* 
 (?P ?I ?C)))
 
;;;; (#$or (#$isa ?I #$BPVLocation)(#$isa ?I #$BPVAgent)(#$isa ?I #$BPVArtifact))
 ;;;;(#$or (#$nameString ?I ?S) (#$webSearchableStrings ?I ?S))

(sim-assert '(#$implies 
(#$and 
 (#$isa ?I #$SimItem)
 (#$termStrings ?I ?S)
 (#$sim:denotesContextually ?S #$SimWorldRez-TopicType ?C))
 (#$ist *MAPPINGMT* (#$isa ?I ?C))) *MAPPINGMT*)

(sim-assert '(#$implies 
(#$and 
 (#$isa ?I #$BPVAgent)
 (#$termStrings ?I ?S) 
 (#$sim:denotesContextually ?S #$SimWorldRez-TopicType ?C))
 (#$ist *MAPPINGMT* (#$isa ?I ?C))) *MAPPINGMT* '(:DIRECTION :FORWARD))

(sim-assert '(#$isa #$TheAtmosphereQuaSinglePieceOfStuff #$FluidTangibleThing) *MAPPINGMT*)
(sim-assert '(#$isa #$TheAtmosphereQuaSinglePieceOfStuff #$PartiallyTangible) *MAPPINGMT*)
(sim-assert '(#$isa #$TheAtmosphereQuaSinglePieceOfStuff #$SpatialThing-Localized) *MAPPINGMT*)


(sim-assert '(#$implies 
(#$and 
;;;; (#$or (#$isa ?I #$BPVLocation)(#$isa ?I #$BPVAgent)(#$isa ?I #$BPVArtifact))
 (#$isa ?I #$SimItem)
 (#$termStrings ?I ?S) 
 (#$sim:denotesContextually ?S #$SimWorldRez-TopicType ?C))
 (#$ist *MAPPINGMT* (#$isa ?I ?C))) *MAPPINGMT* '(:DIRECTION :FORWARD))

 
 


(find-or-create-constant "SimTypeCreatable")
(sim-assert '(#$isa #$SimTypeCreatable #$CollectionType) *UVMT*)
(sim-assert '(#$conceptuallyRelated #$simGenls #$SimTypeCreatable) *MAPPINGMT*)
(sim-assert '(#$implies (#$simGenls ?COL ?HOW) (#$isa ?COL #$SimTypeCreatable)) *MAPPINGMT*)
(sim-assert '(#$comment #$SimTypeCreatable "If something is an instance of #$SimTypeCreatable then it may be rezed)") *UVMT*)

(find-or-create-constant "SimTypePossible")
(sim-assert '(#$isa #$SimTypePossible #$CollectionType) *UVMT*)
(sim-assert '(#$simGenls #$SimTypeCreatable #$SimTypePossible) *UVMT*)
(sim-assert '(#$isa #$GeneralLivingAreaFurnitureMC #$SimTypePossible) *MAPPINGMT*)
(sim-assert '(#$isa #$Decoration #$SimTypePossible) *MAPPINGMT*)
(sim-assert '(#$isa #$Person-SupportingFurniture #$SimTypePossible) *MAPPINGMT*)



(sim-assert '(#$comment #$SimTypePossible "If something is an instance of #$SimType then it's specs may be rezed if they are not #$SimTypeTooGeneral") *UVMT*)
(sim-assert '(#$conceptuallyRelated #$SimTypePossible #$SimTypeCreatable) *MAPPINGMT*)

(exceptWhen (isa ?ARG1 SimTypeExceptions)
 (implies 
   (isa ?ARG1 SimTypePossibles) 
   (isa ?ARG1 SimTypeCreatable)))
	 
;;since i work on an expert system that designs new sims i needed a better target infrastructure. so i am converting the sim3 sim to a SDK that lets you write sims that are more RPG-like. 
whenever i get frustrated i look for a new engine/system ;;P.. even though this is a potental govt contract work, I dont get paid and i have to use free things
looks pretty neat i almost want to switch. sims graphics is not interesting.. mainly its tools that enable devopers to write sims that i care about. droid area system
well my expert system is a webserver app.. so web doesnt bother me
[13:58] <dmiles> oh so only one person plays?
[13:58] <@Uhfgood> essentially you go in build your bot, upload a script, and enter an arena
[13:59] <@Uhfgood> when others enter, then a fight can take place
[13:59] <dmiles> ah.. i should look at their programming language
[13:59] <dmiles> did you have to write a yacc?
[13:59] <@Uhfgood> many people play... in fact it can't exactly be played alone, although you can run a fight simulation, which pits your bot against 3 simulated bots
;;yacc=yet another c commpiler 
one problem is i am doing research that probly wont make me or them money.. if i get government grants sure i dont mind giving them %
	 

(isa 

SimTypeExceptions
 
 (CollectionDifferenceFn SimTypePossibles SimTypeExceptions) 
(implies (isa ?ARG1 SimTypeExceptions)(not (isa ?ARG1 SimTypeCreatable)))

(implies (isa ?ARG1 SimTypePossibles)(or (isa ?ARG1 SimTypeExceptions) (isa ?ARG1 SimTypeCreatable)))

(implies (isa ?ARG1 SimTypeCreatable) (isa ?ARG1 SimTypePossibles))
(implies (isa ?ARG1 SimTypeExceptions) (not (isa ?ARG1 SimTypePossibles)))

(implies (genls ?X Intangible)(isa ?X SimTypeExceptions)) 

(implies (genls ?X SolidTangibleThing)(isa ?X SimTypePossibles)) 

(implies (and (isa ?ARG1 SimTypePossibles) (not (isa ?ARG1 SimTypeExceptions))) (isa ?ARG1 SimTypeCreatable))

(exceptWhen (isa ?ARG1 SimTypeExceptions)(typeGenlss SimTypePossibles SimTypeCreatable))


(implies (isa ?ARG1 (CollectionDifferenceFn SimTypePossibles SimTypeExceptions)) (isa ?ARG1 SimTypeCreatable))

(and 
(isa SymmetricAnatomicalPartType SimTypeExceptions)
(isa UniqueAnatomicalPartType SimTypeExceptions)
(isa AnimalBodyPart SimTypeExceptions)
(isa Currency-US SimTypeExceptions)
(isa Chairman SimTypeExceptions)
(isa Star SimTypeExceptions)
(isa FamousHuman SimTypeExceptions))
(isa Buttocks SimTypeExceptions)
(isa Well SimTypeExceptions)
(isa Healthy SimTypeExceptions)
(isa EllipticalRegion SimTypeExceptions)
(genls SimItem SimTypeExceptions)
(implies (genls ?X SimItem) (ist SimMappingMt (isa ?X SimTypeExceptions)))
(isa DoorInABuilding SimTypeExceptions)
(isa DoorwayCovering SimTypeExceptions)
(isa TallPhysicalBuild SimTypeExceptions)
(isa Block SimTypeExceptions)
(isa CoverOfIBO SimTypeExceptions)
(isa Cover-Protector SimTypeExceptions)
(isa FreeSheet SimTypeExceptions)


(implies 
  (and 
   (isa ?INST BPVLocation) 
   (definiteDescriptions ?INST ?TEXT) 
   (sim:denotationMapper ?TEXT ?STRING ?COL)) 
  (ist SimMappingMt (sim:possibleCycTerms ?INST ?COL)))
  

(sim-assert '(#$typeGenls #$NonessentialAminoAcidType #$SimTypeExceptions) *MAPPINGMT* '(:DIRECTION :FORWARD))
(isa Terrorist SimTypeExceptions)


(sim-assert '(#$isa #$SimTypeCreatable #$FacetingCollectionType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$isa #$SimTypeExceptions #$FacetingCollectionType) *MAPPINGMT* '(:DIRECTION :FORWARD))
;;FacetingCollectionType
  
(implies   
 (and 
;;;; (genls ?COL SimTypePossibles)
 (isa ?GOOD SimTypeCreatable) 
 (isa ?BAD SimTypeExceptions)
 (genls ?COL ?GOOD) 
 (unknownSentence (genls ?COL ?BAD)))
(meetsSomeConstraintOfTopicType ?COL #$SimFoundInLocations-TopicType))  


(implies 
 (and 
  (sim:possibleCycTerms ?INST ?COL)
  (meetsSomeConstraintOfTopicType ?COL #$SimFoundInLocations-TopicType))
 (ist SimMappingMt (relationExistsInstance spatialThingTypeFoundInLocation ?COL ?INST)))

(implies 
 (and 
  (sim:possibleCycTerms ?INST ?COL)
  (meetsSomeConstraintOfTopicType ?COL #$SimWorldRez-TopicType))
 (ist SimMappingMt (relationExistsInstance spatialThingTypeFoundInLocation ?COL ?INST)))
	 

Mt : SimMappingMt
Direction : Forward
(implies 
  (and 
   (isa ?INST BPVLocation) 
   (definiteDescriptions ?INST ?TEXT) 
   (genls ?COL Person-SupportingFurniture) 
   (sim:denotationMapper ?TEXT ?STRING ?COL)) 
  (ist SimMappingMt 
   (relationExistsInstance spatialThingTypeFoundInLocation ?COL ?INST)))
(implies 
  (and 
   (isa ?INST BPVLocation) 
   (definiteDescriptions ?INST ?TEXT) 
   (genls ?COL Decoration) 
   (sim:denotationMapper ?TEXT ?STRING ?COL)) 
  (ist SimMappingMt 
   (relationExistsInstance spatialThingTypeFoundInLocation ?COL ?INST)))
(implies 
  (and 
   (definiteDescriptions ?AREA ?TEXT) 
   (sim:parsesContextually ?TEXT SimWorldRez-TopicType ?CYCL)) 
  (futureAssertion ?AREA ?TEXT ?CYCL))
  

GeneralLivingAreaFurnitureMC Person-SupportingFurniture Decoration


(implies 
  (and 
   (isa ?INST BPVLocation) 
   (definiteDescriptions ?INST ?TEXT) 
   (genls ?COL GeneralLivingAreaFurnitureMC) 
   (sim:denotationMapper ?TEXT ?STRING ?COL)) 
  (ist SimMappingMt 
   (relationExistsInstance spatialThingTypeFoundInLocation ?COL ?INST)))
	 
(implies 
  (and 
   (isa ?INST BPVLocation) 
   (definiteDescriptions ?INST ?TEXT) 
   (sim:relationCandidateExistsAll objectFoundInLocation ?COL BPVLocation) 
   (sim:denotationMapper ?TEXT ?STRING ?COL)) 
  (ist SimMappingMt 
   (relationExistsInstance objectFoundInLocation ?COL ?INST)))

(find-or-create-constant "sim:relationCandidateExistsAll")
(sim-assert '(#$isa #$sim:relationCandidateExistsAll #$TernaryPredicate) *MappingMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$arg1Isa #$sim:relationCandidateExistsAll #$Predicate) *MappingMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$arg2Isa #$sim:relationCandidateExistsAll #$Collection) *MappingMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$arg3Isa #$sim:relationCandidateExistsAll #$Collection) *MappingMt* '(:DIRECTION :FORWARD))



SimTypeCreatable

;;(templateTopicGenls SimFoundInLocations-TopicType SimTypeCreatable)
(find-or-create-constant "SimWorldRez-TopicType")
(sim-assert '(#$isa #$SimWorldRez-TopicType #$FormulaTemplateTopicType) *UVMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$titleForFormulaTemplateType-String #$SimWorldRez-TopicType "Topic template for constraining worldrezing") *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$formulaTemplateTypeHasTopicType #$IndividualTerrainArtifactTemplate #$SimWorldRez-TopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
;;(sim-assert '(#$templateTopicGenls #$SimWorldRez-TopicType #$SimItem) *MAPPINGMT* '(:DIRECTION :FORWARD))
(find-or-create-constant "SimFoundInLocations-TopicType")
(sim-assert '(#$isa #$SimFoundInLocations-TopicType #$FormulaTemplateTopicType) *UVMT* '(:DIRECTION :FORWARD))
;;(sim-assert '(#$isa #$SimFoundInLocations-TopicType #$Collection) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$simGenls #$SimFoundInLocations-TopicType #$SimWorldRez-TopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$titleForFormulaTemplateType-String #$SimFoundInLocations-TopicType "Topic template for constraining objects in rooms") *MAPPINGMT* '(:DIRECTION :FORWARD))
;;(sim-assert '(#$formulaTemplateTypeHasTopicType #$IndividualTerrainArtifactTemplate #$SimFoundInLocations-TopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
;;(sim-assert '(#$templateTopicGenls #$SimFoundInLocations-TopicType #$SimTypeCreatable) *MAPPINGMT* '(:DIRECTION :FORWARD))

 


(implies 
  (and 
   (isa ?INST BPVLocation) 
   (definiteDescriptions ?INST ?TEXT) 
   (sim:denotationMapper ?TEXT ?STRING ?COL)
   (meetsSomeConstraintOfTopicType ?COL #$SimFoundInLocations-TopicType))
  (ist SimMappingMt 
   (relationExistsInstance spatialThingTypeFoundInLocation ?COL ?INST)))
	 



(implies (genls ?X BPVAgent) (ist SimMappingMt (isa ?X SimTypeExceptions)))
)
;;;; EllipticalRegion 

;;(isa Channel-BodyOfWaterLink SimTypeExceptions)
;; spatialThingTypeFoundInLocation Decoration 

AllEnglishLexicalMicrotheoryPSC
AllEnglishTemplateMt
(define pasc (x) (parse-a-question-completely x #$AllEnglishLexicalMicrotheoryPSC))
(pasc "who do you love?")




(find-or-create-constant "SimTypeTooGeneral")
(sim-assert '(#$isa #$SimTypeTooGeneral #$CollectionType) *UVMT*)
(sim-assert '(#$disjointWith #$SimTypeTooGeneral #$SimTypeCreatable) *MAPPINGMT*)
;;;;(sim-assert '(#$implies (#$simGenls ?COL ?HOW) (#$isa ?COL #$SimTypeCreatable)) *MAPPINGMT*)
(sim-assert '(#$comment #$SimTypeTooGeneral "If something is an instance of #$SimTypeTooGeneral then it may not be rezed)") *UVMT*)

(sim-assert '(#$simGenls (#$CollectionDifferenceFn #$SimType #$SimTypeTooGeneral) #$SimTypeCreatable) *UVMT*)

(sim-assert '(#$typeGenls #$SimTypeCreatable (#$CollectionDifferenceFn #$SimType #$SimTypeTooGeneral)) *UVMT*)
(sim-assert '(#$collectionConventionMt #$SimTypeCreatable *MAPPINGMT*) *UVMT*)
(sim-assert '(#$collectionConventionMt #$SimTypeTooGeneral *MAPPINGMT*) *UVMT*)
(sim-assert '(#$collectionConventionMt #$SimType *MAPPINGMT*) *UVMT*)


(find-or-create-constant "simViolatesSomeConstraintIsa")
(sim-assert '(#$isa #$simViolatesSomeConstraintIsa #$BinaryPredicate) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$comment #$simViolatesSomeConstraintIsa "The all instances of :ARG1 violates the constraint in :ARG2") *MAPPINGMT* '(:DIRECTION :FORWARD))
(find-or-create-constant "simViolatesSomeGenlsConstraint")
(sim-assert '(#$isa #$simViolatesSomeConstraintGenl #$BinaryPredicate) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$comment #$simViolatesSomeConstraintGenl "The all specs of :ARG1 violates the constraint in :ARG2") *MAPPINGMT* '(:DIRECTION :FORWARD))
(find-or-create-constant "simViolatesSomeConstraint")
(sim-assert '(#$isa #$simViolatesSomeConstraint #$BinaryPredicate) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$comment #$simViolatesSomeConstraint "The :ARG1 violates the constraint in :ARG2") *MAPPINGMT* '(:DIRECTION :FORWARD))
(find-or-create-constant "simExceedsSomeConstraintIsa")
(sim-assert '(#$isa #$simExceedsSomeConstraintIsa #$BinaryPredicate) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$comment #$simExceedsSomeConstraintIsa "When something is an instance of :ARG1 exceeds the constraint in :ARG2 and is a prefered example") *MAPPINGMT* '(:DIRECTION :FORWARD))
(find-or-create-constant "simExceedsSomeGenlsConstraint")
(sim-assert '(#$isa #$simExceedsSomeConstraintGenl #$BinaryPredicate) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$comment #$simExceedsSomeConstraintGenl "When something is an instance of :ARG1 exceeds the constraint in :ARG2 and is a prefered example") *MAPPINGMT* '(:DIRECTION :FORWARD))
(find-or-create-constant "simExceedsSomeConstraint")
(sim-assert '(#$isa #$simExceedsSomeConstraint #$BinaryPredicate) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$comment #$simExceedsSomeConstraint "The :ARG1 exceeds the constraint in :ARG2 and is a prefered example") *MAPPINGMT* '(:DIRECTION :FORWARD))


(sim-assert '(#$arg1Isa #$simViolatesSomeConstraintIsa #$Collection) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$arg1Isa #$simViolatesSomeConstraintGenl #$Collection) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$arg1Isa #$simViolatesSomeConstraint #$Thing) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$arg1Isa #$simExceedsSomeConstraintIsa #$Collection) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$arg1Isa #$simExceedsSomeConstraintGenl #$Collection) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$arg1Isa #$simExceedsSomeConstraint #$Thing) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$arg2Isa #$simViolatesSomeConstraintIsa #$FormulaTemplateTopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$arg2Isa #$simViolatesSomeConstraintGenl #$FormulaTemplateTopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$arg2Isa #$simViolatesSomeConstraint #$FormulaTemplateTopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$arg2Isa #$simExceedsSomeConstraintIsa #$FormulaTemplateTopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$arg2Isa #$simExceedsSomeConstraintGenl #$FormulaTemplateTopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$arg2Isa #$simExceedsSomeConstraint #$FormulaTemplateTopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$completeExtentDecidable #$simViolatesSomeConstraint) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$completeExtentDecidable #$simExceedsSomeConstraint) *MAPPINGMT* '(:DIRECTION :FORWARD))


(sim-assert '(#$negationPreds #$simExceedsSomeConstraint #$simViolatesSomeConstraint) *MAPPINGMT* '(:DIRECTION :FORWARD))
(sim-assert '(#$implies (#$simExceedsSomeConstraint ?P #$SimWorldRez-TopicType)(#$meetsSomeConstraintOfTopicType ?P ?TT)) *MAPPINGMT*)
(sim-assert '(#$implies (#$and (#$simExceedsSomeConstraintIsa ?W ?TT)(#$isa ?P ?W)) (#$simExceedsSomeConstraint ?P ?TT)) *MAPPINGMT*)

;;(sim-assert '(#$implies (#$simGenls ?P ?W) (#$simExceedsSomeConstraint ?P #$SimWorldRez-TopicType)) '*MappingMt*)

(sim-assert '(#$implies (#$not (#$simViolatesSomeConstraint ?P ?TT))(#$meetsSomeConstraintOfTopicType ?P ?TT)) *MAPPINGMT*)
(sim-assert '(#$implies (#$simExceedsSomeConstraint ?P #$SimWorldRez-TopicType) (#$meetsSomeConstraintOfTopicType ?P ?TT)) *MAPPINGMT*)
(sim-assert '(#$implies (#$and (#$simExceedsSomeConstraintGenl ?W ?TT)(#$simGenls ?P ?W)) (#$simExceedsSomeConstraint ?P ?TT)) *MAPPINGMT*)
;;(sim-assert '(#$implies (#$simViolatesSomeConstraintIsa ?COL ?ANYWHERE) (#$ist *MAPPINGMT* (#$typeGenls ?COL #$SimTypeExceptions))) *MAPPINGMT* '(:DIRECTION :FORWARD))
;;(sim-assert '(#$implies (#$simViolatesSomeConstraintGenl ?COL ?ANYWHERE) (#$ist *MAPPINGMT* (#$simGenls ?COL #$SimTypeExceptions))) *MAPPINGMT* '(:DIRECTION :FORWARD))
;;(sim-assert '(#$implies (#$simViolatesSomeConstraint ?COL ?ANYWHERE) (#$ist *MAPPINGMT* (#$isa ?COL #$SimTypeExceptions))) *MAPPINGMT* '(:DIRECTION :FORWARD))
;;(sim-assert '(#$implies (#$and (#$simGenls ?C ?U)(#$simViolatesSomeConstraint ?U)) (#$simViolatesSomeConstraint ?C)) *MAPPINGMT*)
;;(sim-assert '(#$implies (#$and (#$simViolatesSomeConstraintIsa ?Super ?TT)(#$isa ?Sub ?Super))(#$simViolatesSomeConstraint ?Sub ?TT)) *MAPPINGMT*)
;;(sim-assert '(#$implies (#$and (#$simViolatesSomeConstraintGenl ?Super ?TT)(#$simGenls ?Sub ?Super))(#$simViolatesSomeConstraint ?Sub ?TT)) *MAPPINGMT*)
;;(sim-assert '(#$implies (#$and (#$simViolatesSomeConstraint ?U)(#$simGenls ?C ?U)) (#$not (#$simExceedsSomeConstraint ?C))) *MAPPINGMT* '(:DIRECTION :FORWARD))
;;(cyc-unassert '(#$implies (#$simViolatesSomeConstraintIsa ?COL ?ANYWHERE) (#$typeGenls #$SimTypeExceptions ?COL)) *MAPPINGMT*)
;;(cyc-unassert '(#$implies (#$simViolatesSomeConstraintIsa ?COL ?ANYWHERE) (#$simGenls #$SimTypeExceptions ?COL)) *MAPPINGMT*)
;;(cyc-unassert '(#$implies (#$simViolatesSomeConstraint ?COL ?ANYWHERE) (#$isa ?COL #$SimTypeExceptions)) *MAPPINGMT*)

(find-or-create-constant "simRezClass")
(sim-assert '(#$isa #$simRezClass #$TernaryPredicate) *UVMT*)
(sim-assert '(#$comment #$simRezClass "(#$simRezClass ?Arg1 ?Arg2 ?P) will return #$isa or #$simGenls or some other mostly legal reation between the two arguments") *UVMT*)
(sim-assert '(#$isa #$simRezClass #$RemovalModuleSupportedPredicate-Specific) #$CycAPIMt)
(sim-assert '(#$arity #$simRezClass 3) *UVMT*)
(sim-assert '(#$arg1Isa #$simRezClass #$Thing) *UVMT*)
(sim-assert '(#$arg2Genl #$simRezClass #$ThreeDimensionalThing) *UVMT*)
(sim-assert '(#$arg2Genl #$simRezClass #$SomethingExisting) *UVMT*)
(sim-assert '(#$arg3Isa #$simRezClass #$BinaryPredicate) *UVMT*)
(sim-assert '(#$conceptuallyRelated #$simRezClass #$simViolatesSomeConstraint) *UVMT*)
(inference-removal-module :removal-rezPredicate-bound-bound-unbound
 '(:sense :pos 
	:predicate #$simRezClass 
	:required-pattern (#$simRezClass :fully-bound :fully-bound :not-fully-bound) 
	:cost-expression 0 :completeness :complete 
	:input-extract-pattern (:template (#$simRezClass (:bind value-1) (:bind value-2) :anything) ((:value value-1) (:value value-2)))
	:input-verify-pattern :anything
	:output-generate-pattern (:call rezPredicate :input)
	:output-construct-pattern (#$simRezClass (:value value-1) (:value value-2) :input)))
(register-solely-specific-removal-module-predicate #$simRezClass)
(define rezPredicate (vs)
 (clet ((v1 (car vs)) (v2 (nth 1 vs)))
 (cand (cq (list #$isa v2 #$Collection)) (null (cq (list #$simViolatesSomeConstraint v2)))
 (ret (list (fif (cq (list #$isa v1 #$Collection)) #$simGenls #$isa))))))


(sim-assert '(#$ksTermString #$MarinePersonnel "ensign") *MappingMt*)
(sim-assert '(#$ksTermString #$Leader "Commander") *MappingMt*)
(sim-assert '(#$ksTermString #$SignTheDisplay "sign") *MappingMt*)
(sim-assert '(#$ksTermString #$Elevator "Turbolift") *MappingMt*)
(sim-assert '(#$ksTermString #$Bottle "bottle") *MappingMt*)


(sim-assert '(#$simViolatesSomeIsaConstraint #$VehiclePart #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeIsaConstraint #$Relation #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeIsaConstraint #$Microtheory #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeIsaConstraint #$Individual #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeIsaConstraint #$DurativeEventType #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeIsaConstraint #$SimItem #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeIsaConstraint #$DisjointCollectionType #$SimWorldRez-TopicType) '*MappingMt*) 
(sim-assert '(#$simViolatesSomeIsaConstraint #$DerivedMeasurableQuantityType #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeIsaConstraint #$CollectionType #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeIsaConstraint #$BPVItemType #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeIsaConstraint #$BPVAgentType #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeIsaConstraint #$AnimalActivity #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeIsaConstraint #$Action #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeIsaConstraint #$AccomplishmentType #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeGenlsConstraint #$VolkswagenTransporterCar #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeGenlsConstraint #$Specification #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeGenlsConstraint #$SocialOccurrence #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeGenlsConstraint #$Sick #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeGenlsConstraint #$ShoeUppers #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeGenlsConstraint #$Pipeline #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeGenlsConstraint #$Organization #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeGenlsConstraint #$Nucleotide #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeGenlsConstraint #$Movement-TranslationProcess #$SimWorldRez-TopicType) '*MappingMt*) 
(sim-assert '(#$simViolatesSomeGenlsConstraint #$Microtheory #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeGenlsConstraint #$Intangible #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeGenlsConstraint #$Hair-Stuff #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeGenlsConstraint #$Group #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeGenlsConstraint #$GatlingGun-GAU4 #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeGenlsConstraint #$Event-Organized #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeGenlsConstraint #$Event #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeGenlsConstraint #$SimItem #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeGenlsConstraint #$Criterion #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeGenlsConstraint #$Bay #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeGenlsConstraint #$BPVItemType #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeGenlsConstraint #$BPVAgentType #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeGenlsConstraint #$AnimalActivity #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeGenlsConstraint #$AminoAcid #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeGenlsConstraint #$Action #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeGenlsConstraint #$Abnormal-Unusual #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeConstraint (#$MeatFn #$EdibleFish) #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeConstraint (#$GroupFn #$Fish) #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeConstraint #$TrunkOfCar #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeConstraint #$Stimulant #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeConstraint #$SpottedPattern #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeConstraint #$QuarterCoin-US #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeConstraint #$OneDollarBill-US #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeConstraint #$Ocean #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeConstraint #$Issue #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeConstraint #$InformationTransferEvent #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeConstraint #$Gold #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeConstraint #$Flag #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeConstraint #$Fisherman #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeConstraint #$EthnicGroupOfBlacks #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeConstraint #$Diligent #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeConstraint #$Criterion #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeConstraint #$Communicating #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeConstraint #$Command #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeConstraint #$BootingAComputer #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeConstraint #$Black-HairColor #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeConstraint #$Amphetamine #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeConstraint #$AfricanAmericanPerson #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simViolatesSomeConstraint #$AcademicQuarter #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simExceedsSomeIsaConstraint #$ChromaticColor #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simExceedsSomeGenlsConstraint #$PartOfBuilding #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simExceedsSomeConstraint #$AirplaneCockpit #$SimWorldRez-TopicType) '*MappingMt*)
(sim-assert '(#$simExceedsSomeConstraint #$AirplaneCargoBay #$SimWorldRez-TopicType) '*MappingMt*)
;;(sim-assert '(#$simExceedsSomeConstraint #$DrinkingGlass) '*MappingMt* '(:DIRECTION :FORWARD))
|#

"worldmodel_soulcube"
"worldmodel_shotgun"
"worldmodel_rocketlauncher"
"worldmodel_plasmagun"
"worldmodel_pistol"
"worldmodel_pda"
"worldmodel_machinegun"
"worldmodel_handgrenade"
"worldmodel_flashlight"
"worldmodel_chainsaw"
"worldmodel_chaingun"
"worldmodel_bfg"
"viewmodel_soulcube"
"viewmodel_shotgun"
"viewmodel_rocketlauncher"
"viewmodel_plasmagun"
"viewmodel_pistol"
"viewmodel_pda"
"viewmodel_machinegun"
"viewmodel_grenade"
"viewmodel_flashlight"
"viewmodel_fists"
"viewmodel_chainsaw"
"viewmodel_chaingun"
"viewmodel_bfg"
"underground_zsecshotgun"
"underground_zombie_maint_bald"
"underground_zintro_zombie"
"underground_zintro_victim"
"underground_zintro_pop"
"underground_window_security"
"underground_tunnel_imp"
"underground_security_helmet_mark"
"underground_mcu_pipes"
"underground_maint_young_daniel_head"
"underground_maint_young_daniel"
"underground_maint_old_charles_head"
"underground_maint_old_charles"
"underground_maint_bald_ross_head"
"underground_maint_bald_ross"
"underground_maint_asian_eric_head"
"underground_maint_asian_eric2"
"underground_maint_asian_eric"
"underground_labcoat_young_todd"
"underground_invasion_zsechead"
"underground_invasion_suit"
"underground_invasion_smallerskulls"
"underground_invasion_security"
"underground_invasion_sechead"
"underground_invasion_pentagram"
"underground_invasion_maintzombie"
"underground_invasion_littleskulls"
"underground_invasion_lightattacher"
"underground_invasion_ledge"
"underground_invasion_labzombie"
"underground_invasion_labcoat_head"
"underground_invasion_labcoat"
"underground_invasion_chestskull"
"underground_invasion_bigskull"
"underground_impstairs"
"underground_impintro_imp"
"underground_impcrawl_door"
"underground_fatty"
"underground_crazy_zombie"
"underground_crazy_zombhead"
"underground_crazy_stump"
"underground_crazy_scihead"
"underground_crazy_sci"
"underground_cin_player2"
"underground_cin_player"
"underground_cin_pistol"
"underground_choke_zsechead"
"underground_choke_zombie"
"underground_choke_victim"
"underground_brooks_head"
"test_soldier"
"test_labcoat_black"
"ss_trite4"
"ss_trite3"
"ss_trite2"
"ss_trite"
"ss_pcg_player"
"ss_pcg_hk"
"ss_imp_gamepro"
"ss_hk_gamepro"
"ss_hero_player"
"ss_hellknight"
"ss_ew_maint"
"ss_ew_jump"
"ss_ew_fat"
"ss_cgw_zsci2"
"ss_cgw_zsci"
"ss_cgw_jump"
"ss_cgw_gibs"
"space_stars"
"space_ship"
"space_planet"
"site3_sci_head"
"site3_sci"
"site3_hellknight"
"site3_floor"
"recycling2_video_betruger"
"recycling2_video_bethead"
"recycling2_mancintro_player"
"recycling2_mancintro_pipes"
"recycling2_mancintro_manc"
"recycling2_imp3_cin"
"recycling2_imp2_cin"
"recycling2_imp1_cin"
"recycling1_skybridge"
"recycling1_revintro_rev"
"recycling1_revintro_player2"
"recycling1_revintro_player"
"recycling1_revintro_impact"
"prop_wrench"
"prop_tabletpc"
"prop_soft_desk_chair2"
"prop_soft_desk_chair1"
"prop_shotgun"
"prop_pistol"
"prop_pda"
"prop_metal_lounge_chair"
"prop_metal_kitchen_chair"
"prop_machinegun"
"prop_lftflashlight"
"prop_foamcup"
"prop_flashlight"
"prop_dufflebag"
"prop_cola"
"prop_big_cushy_chair"
"prop_adrenaline"
"npc_tshirt"
"npc_suit2"
"npc_suit"
"npc_skeleton"
"npc_security"
"npc_player"
"npc_marine_w_mgun"
"npc_marine"
"npc_labcoat"
"npc_jumpsuit"
"npc_hazmat"
"npc_base"
"monster_zombie_zfem"
"monster_zombie_wrench"
"monster_zombie_suit"
"monster_zombie_soldier"
"monster_zombie_maint_fast"
"monster_zombie_maint"
"monster_zombie_labcoat_hanging"
"monster_zombie_labcoat"
"monster_zombie_jumpy"
"monster_zombie_jumpsuit_old"
"monster_zombie_jumpsuit_influence"
"monster_zombie_jumpsuit"
"monster_zombie_fat_wrench"
"monster_zombie_fat_eating"
"monster_zombie_fat"
"monster_zombie_commando_cgun"
"monster_zombie_commando"
"monster_zombie_chainsaw"
"monster_zombie_boney"
"monster_zombie_bernie"
"monster_turret"
"monster_flying_lostsoul"
"monster_flying_cacodemon"
"monster_demon_wraith"
"monster_demon_vulgar"
"monster_demon_trite"
"monster_demon_tick"
"monster_demon_sentry"
"monster_demon_revenant"
"monster_demon_pinky_pipes"
"monster_demon_pinky"
"monster_demon_mancubus"
"monster_demon_maggot"
"monster_demon_imp_crawler"
"monster_demon_imp_crawl_armdoor"
"monster_demon_imp"
"monster_demon_hellknight"
"monster_demon_d3xp_bruiser"
"monster_demon_cherub"
"monster_demon_archvile"
"monster_boss_vagary"
"monster_boss_sabaoth"
"monster_boss_maledict_cinematic"
"monster_boss_guardian_seeker"
"monster_boss_guardian"
"monster_boss_cyberdemon"
"monorail_riderhead"
"monorail_rider"
"monorail_raisecommando_zct"
"monorail_raisecommando_marine1"
"monorail_raisecommando_changer"
"monorail_raisecommando_betrugerhead"
"monorail_raisecommando_betruger"
"monorail_crashsight_player"
"monorail_crash_debris"
"model_sp_marine"
"model_mp_marine"
"model_monster_zsec_shotgun"
"model_monster_zsec_shield"
"model_monster_zsec_pistol_slowfire"
"model_monster_zsec_pistol"
"model_monster_zsec_machinegun"
"model_monster_zombie_sawyer"
"model_monster_zombie_morgue"
"model_monster_zombie_flashlight"
"model_monster_zombie_camzombie"
"model_monster_zombie"
"model_delta2a_zombie_nojaw"
"marscity_walking_swann"
"marscity_suit_young_chair"
"marscity_suit_young2"
"marscity_suit_officeguy1"
"marscity_suit_chair"
"marscity_suit_asian_chair_head2"
"marscity_suit_asian_chair_head"
"marscity_suit_asian_chair2"
"marscity_suit_asian_chair"
"marscity_soldier_young_pda_head"
"marscity_soldier_young_pda"
"marscity_soldier_black_head"
"marscity_soldier_black"
"marscity_soldier_bald_pda_head"
"marscity_soldier_bald_pda"
"marscity_smallpda"
"marscity_ship3"
"marscity_ship2"
"marscity_ship"
"marscity_security_machinegun"
"marscity_security_goggles_pda_head"
"marscity_security_goggles_pda"
"marscity_sec_window2_head"
"marscity_sec_window2"
"marscity_sec_window"
"marscity_sec_checkin_head"
"marscity_sec_checkin"
"marscity_sci_team_request_head"
"marscity_sci_team_request"
"marscity_receptionist_head3"
"marscity_receptionist_head"
"marscity_receptionist3"
"marscity_receptionist2"
"marscity_receptionist"
"marscity_marine_helmet_p90_walking"
"marscity_marine_helmet_p90_directions_ver2"
"marscity_marine_helmet_p90_directions_ver1"
"marscity_maint2_hallway_head"
"marscity_maint2_hallway"
"marscity_maint1_hallway_head"
"marscity_maint1_hallway"
"marscity_labcoat_monitor_head"
"marscity_labcoat_monitor"
"marscity_labcoat_black"
"marscity_ipn_news_head"
"marscity_ipn_news"
"marscity_head_swann"
"marscity_head_sarge"
"marscity_head_player"
"marscity_head_campbell"
"marscity_hangar_swann"
"marscity_hangar_player"
"marscity_hangar_campbell"
"marscity_hangar"
"marscity_dufflebag"
"marscity_civilian_kitchen_head"
"marscity_civilian_kitchen"
"marscity_civilian_hallway"
"marscity_civilian_bathroom_head"
"marscity_civilian_bathroom"
"marscity_civilian2"
"marscity_civilian1_head"
"marscity_civilian1"
"marscity_cinematic_victim"
"marscity_cinematic_swann"
"marscity_cinematic_security"
"marscity_cinematic_sarge2"
"marscity_cinematic_sarge"
"marscity_cinematic_player"
"marscity_cinematic_head_sarge2"
"marscity_cinematic_doctor"
"marscity_cinematic_campbell"
"marscity_cinematic_betruger"
"marscity_char_maint_asian_pda_head"
"marscity_char_maint_asian_pda"
"marscity_betruger_head"
"marscity2_zombie_morgue"
"marscity2_video_sarge"
"marscity2_video_head_sarge"
"marscity2_maint_ceiling"
"marscity2_imp"
"marscity2_hallway_zombie"
"marscity2_hallway_marine"
"marscity2_cin_imp"
"marscity2_cin_fatty"
"marscity2_ceiling_head"
"maledict_world"
"maledict_debris1"
"intro_scientist"
"intro_pinky"
"intro_panel"
"intro_meat"
"hellhole_walkmarine"
"hellhole_swann_head"
"hellhole_swann"
"hellhole_soulcube"
"hellhole_pdamarine"
"hellhole_maggot"
"hellhole_lookmarine"
"hellhole_imp"
"hellhole_cyberdeathrocks"
"hellhole_cyberbricks"
"hellhole_coffinbricks"
"hellhole_cin_player3"
"hellhole_cin_player2"
"hellhole_cin_player"
"hellhole_cin_npcplayerhead"
"hellhole_cin_npcplayer"
"hellhole_cin_marine"
"hellhole_cin_cyberdemon2"
"hellhole_cin_cyberdemon"
"hell_rstep"
"hell_lstep"
"hell1_soulcube"
"hell1_monster_boss_guardian"
"hell1_cin_player"
"head_zombie6"
"head_zombie5"
"head_zombie4"
"head_zombie3"
"head_zombie2"
"head_zombie1"
"head_young"
"head_swann"
"head_surgeon"
"head_stump"
"head_skull"
"head_security_helmet"
"head_security_goggles"
"head_security_dhelmet"
"head_sarge"
"head_pretty_old"
"head_player"
"head_old"
"head_marine_helmet"
"head_campbell"
"head_bloodybald"
"head_black"
"head_betruger"
"head_bald"
"head_asian_young"
"head_asian_old"
"env_zsci_corpse"
"env_xianbutton_3"
"env_xianbutton_2"
"env_xianbutton_1"
"env_wirec"
"env_wireb"
"env_wirea"
"env_web"
"env_swinglight_sl64"
"env_swinglight_sl128"
"env_swinglight_round"
"env_swinglight_long_wbulbs_32"
"env_swinglight_long_wbulbs_192"
"env_swinglight_long_wbulbs_128"
"env_swinglight_long_wbulbs"
"env_swinglight_long_fixed"
"env_swinglight_long"
"env_swing_tubearm"
"env_storagecabinet_openback"
"env_storagecabinet"
"env_steampipe_c"
"env_steampipe_b"
"env_steampipe_a"
"env_skullgate"
"env_severed_zombie"
"env_sencpu"
"env_rope"
"env_reactor"
"env_ragdoll_zsecs"
"env_ragdoll_zsecp"
"env_ragdoll_zsecm"
"env_ragdoll_zscientist3"
"env_ragdoll_zscientist2"
"env_ragdoll_zscientist1"
"env_ragdoll_zmaint"
"env_ragdoll_zfem"
"env_ragdoll_tentacle"
"env_ragdoll_suit3"
"env_ragdoll_suit2"
"env_ragdoll_suit1"
"env_ragdoll_skeleton2"
"env_ragdoll_skeleton"
"env_ragdoll_sentry"
"env_ragdoll_revenant"
"env_ragdoll_poppy"
"env_ragdoll_pinky"
"env_ragdoll_mummy"
"env_ragdoll_marine_stump"
"env_ragdoll_marine_helmet"
"env_ragdoll_marine2"
"env_ragdoll_marine1"
"env_ragdoll_maint"
"env_ragdoll_lab4"
"env_ragdoll_lab3"
"env_ragdoll_lab2"
"env_ragdoll_lab1"
"env_ragdoll_imp"
"env_ragdoll_hellknight"
"env_ragdoll_hazmat"
"env_ragdoll_fatty"
"env_ragdoll_commando"
"env_ragdoll_cherub"
"env_ragdoll_boney"
"env_ragdoll_archvile"
"env_playerandallguns"
"env_player_simmarine"
"env_pipe_96"
"env_pinky_pipes"
"env_orangesteampipe_128"
"env_locust"
"env_juglamp"
"env_inviso"
"env_implanding"
"env_hires_steampipe"
"env_hellchain"
"env_hellcage_short"
"env_hellcage_medium"
"env_hellcage_long"
"env_greysteampipe_128"
"env_gibs_torso"
"env_gibs_spine"
"env_gibs_ruparmstub"
"env_gibs_rtuplegstump"
"env_gibs_rtlolegstump"
"env_gibs_leftleg"
"env_gibs_leftarm"
"env_evilmeat"
"env_dragtest"
"env_curtain"
"env_craneplatform_mcunderground"
"env_craneplatform_caverns1"
"env_cranegame"
"env_cage_corpse"
"env_buggy"
"env_bluesteampipe_128"
"env_bfgcase"
"env_airlockdoor"
"enpro_wounded_player"
"enpro_wounded_marine2"
"enpro_wounded_marine"
"enpro_wounded_head2"
"enpro_wounded_head"
"enpro_swannhead"
"enpro_swann"
"enpro_soldier4"
"enpro_soldier3"
"enpro_soldier2"
"enpro_soldier1"
"enpro_lost_spine"
"enpro_lost_chair"
"enpro_keycard"
"enpro_exit_imp2"
"enpro_exit_imp1"
"enpro_exit_helmet"
"enpro_cin_wraith"
"enpro_cin_player"
"enpro_cin_machinegun"
"enpro_cin_lostsoul"
"enpro_cin_female"
"enpro_campbellhead"
"enpro_campbell"
"ending_debris"
"delta5_wounded_swann"
"delta5_swann_head"
"delta4_hazguy"
"delta4_cin_teleporter"
"delta4_cin_player"
"delta4_cin_hk"
"delta4_cin_hazguy"
"delta4_betruger_head"
"delta4_betruger"
"delta3_betruger_head"
"delta3_betruger"
"delta2b_waterwires"
"delta2b_tankrevwires"
"delta2b_tankrev"
"delta2b_tankimpwires"
"delta2b_tankimp"
"delta2b_keycardzombie"
"delta2b_imp"
"delta2b_hazmat"
"delta2a_teleporter"
"delta2a_scientist_return"
"delta2a_scientist_head_return"
"delta2a_scientist_head"
"delta2a_scientist"
"delta2a_player"
"delta2a_imp"
"delta2a_fatty"
"delta2a_commando"
"delta2a_cin_fatty"
"delta1_scipull"
"delta1_sarge_video_head"
"delta1_sarge_video"
"delta1_reactor"
"delta1_hellknight"
"mud_agenthead"
"mud_agent"
"cpuboss_cin_sabaoth"
"cpuboss_cin_player"
"cpu1_wounded_campbell"
"cpu1_monster_trite"
"cpu1_head_campbell"
"cpu1_camphunt_campbell"
"commoutside_vehicle"
"commoutside_swann"
"commoutside_player"
"commoutside_hellgoo1"
"commoutside_fatty"
"commoutside_cin_sawyer"
"commoutside_campbell"
"comm1_video_swannhead"
"comm1_video_swann"
"comm1_sentryloader"
"comm1_sentry_blank"
"comm1_sentry"
"cin_zombie_drag"
"cin_toob"
"cin_tele_skull7"
"cin_shake"
"cin_posess_zsec"
"cin_posess_skull3"
"cin_posess_skull2"
"cin_posess_skull"
"cin_posess_sec"
"cin_posess_imp"
"cin_posess_hk"
"cin_pink_drag"
"cin_mars_flare"
"cin_hkrip_uac"
"cin_hkendroar_hk"
"cin_hkdeath_uac"
"cin_hk_walkbydoor"
"cin_hk_hk"
"cin_fat_drag"
"cin_elevator_player"
"cin_elevator_elev"
"cin_e3teaser_hk"
"cin_chain_zct"
"cin_chain_zcc"
"cin_chain_uacm"
"cin_anim9a_uacm"
"cin_anim9a_pinky"
"cin_anim9a_fatty"
"cin_anim5_pinky"
"cin_anim5_fatty"
"cin_anim3_pinky"
"cin_anim3_fatty"
"cin_anim2_pinky"
"cin_anim2_fatty"
"cin_anim1_uacm"
"cin_anim1_pinky"
"cin_anim1_fatty"
"char_swann"
"char_sentry_flashlight"
"char_sentry"
"char_security_goggles_pistol"
"char_poppy"
"char_marine_young_chair1_head"
"char_marine_young_chair1"
"char_marine_asian_chair1_head"
"char_marine_asian_chair1"
"char_labcoat_black"
"char_hazmat"
"char_campbell_bfgcase"
"char_campbell_bfg"
"char_campbell"
"char_betruger"
"caverns_vagrocks"
"caverns_vagary2"
"caverns_vagary1"
"caverns_pinky"
"caverns_imp2"
"caverns_imp1"
"caverns_hkwall"
"caverns_hellknight2"
"caverns_hellknight"
"caverns_char_maint_scared_head"
"caverns_char_maint_scared"
"caverns_bridgefront"
"caverns_bridgecrane"
"caverns_bridgeback"
"caverns_bridge"
"caverns_boulderbridge"
"blooper_labcoat"
"blooper_hazguy"
"blooper_betrugerhead"
"alphalabs3_vagaryintro_vagary"
"alphalabs3_vagaryintro_player"
"alphalabs3_fragchamber_skeleton"
"alphalabs3_fragchamber_scientist_head"
"alphalabs3_fragchamber_scientist2_head"
"alphalabs3_fragchamber_scientist2"
"alphalabs3_fragchamber_scientist"
"alphalabs2_scientist1_head"
"alphalabs2_scientist1"
"alphalabs2_marine_helmet_p90"
"alphalabs2_maggot"
"alphalabs2_imp"
"alphalabs2_elevatorenv"
"alphalabs2_cin_imp"
"alphalabs1_player"
"alphalabs1_pentagramdood"
"alphalabs1_maggot3"
"alphalabs1_maggot2"
"alphalabs1_maggot1"
"alphalabs1_labcoat_bald_head"
"alphalabs1_labcoat_bald"
"alphalabs1_imp"
"alphalabs1_helldoll"
"admin_wounded_marine_head"
"admin_wounded_marine"
"admin_vent_swann"
"admin_railing_pinkyattack"
"admin_pinkyattack_shards"
"admin_pinkyattack_pinky"
"admin_pinky_glassbreak"
"admin_overhear_swannhead"
"admin_overhear_swann"
"admin_overhear_player"
"admin_overhear_camphead"
"admin_overhear_campbell_bfg"
"admin_overhear_campbell2"
"admin_overhear_campbell"
"admin_impdoors"
"admin_imp2"
"admin_imp1"
"admin_doorframe_pinkyattack"
"admin_bfgcase"
