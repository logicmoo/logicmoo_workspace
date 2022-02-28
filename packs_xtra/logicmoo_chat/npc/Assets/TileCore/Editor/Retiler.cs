using System;
using UnityEditor;
using UnityEngine;
using Object = UnityEngine.Object;

public static class Retiler
{
    private static TileMap tileMap;

    [MenuItem("TileMap/Retile")]
    public static void RetileMap()
    {
        tileMap = Object.FindObjectOfType<TileMap>();
        tileMap.RebuildMap();
        for (int row = 0; row < tileMap.MapRows; row++)
            for (int column = 0; column < tileMap.MapColumns; column++)
            {
                var tilePosition = new TilePosition(column, row);
                var newSprite = TileSpriteAt(tilePosition);
                if (newSprite != null)
                    tileMap.SetTileSprite(tilePosition, newSprite);
            }
        Debug.Log("Reset background tiles.");
    }

    [Flags]
    enum Direction {
        None = 0,
        Up = 1,
        Down = 2,
        Left = 4,
        Right = 8, 
        UpLeft = 16, 
        UpRight = 32,
        DownLeft = 64, 
        DownRight = 128
    };

    private static Sprite TileSpriteAt(TilePosition tilePosition)
    {
        var wallStyle = Object.FindObjectOfType<WallTopStyle>();

        var r = tileMap.TileRoom(tilePosition);
        if (r != null)
        {
            if (r.Wall != null)
            {
                if (r.WithinPortal(tilePosition) && tileMap.TileRoom(tilePosition.Up) == null)
                    // It's the top tile of a vertical portal between two rooms; make it be a wall
                    return r.Wall.SingletonSprite;

                if (tilePosition.Row == r.Bottom + r.Height - 1 && !r.IsBackWallTile(tilePosition))
                    // It's the back wall
                {
                    if (r.WithinPortal(tilePosition.Right.Up))
                        return r.Wall.RightSprite;
                    if (r.WithinPortal(tilePosition.Left.Up))
                        return r.Wall.LeftSprite;
                    return r.Wall.CenterSprite;
                }
            }
            return r.Floor.Sprite;
        }

        // The tile isn't the interiod of a room.
        // Check to see if it's adjacent to a room.
        var upRoom = TileMap.TheTileMap.TileRoom(tilePosition.Down);
        var downRoom = TileMap.TheTileMap.TileRoom(tilePosition.Up);
        var leftRoom = TileMap.TheTileMap.TileRoom(tilePosition.Left);
        var rightRoom = TileMap.TheTileMap.TileRoom(tilePosition.Right);
        var upLeftRoom = TileMap.TheTileMap.TileRoom(tilePosition.Down.Left);
        var upRightRoom = TileMap.TheTileMap.TileRoom(tilePosition.Down.Right);
        var downLeftRoom = TileMap.TheTileMap.TileRoom(tilePosition.Up.Left);
        var downRightRoom = TileMap.TheTileMap.TileRoom(tilePosition.Up.Right);


        var neighborsInRooms = (upRoom?Direction.Up:0)
            | (downRoom?Direction.Down:0)
            | (leftRoom ? Direction.Left : 0)
            | (rightRoom ? Direction.Right : 0)
            | (upLeftRoom ? Direction.UpLeft : 0)
            | (upRightRoom ? Direction.UpRight : 0)
            | (downLeftRoom ? Direction.DownLeft : 0)
            | (downRightRoom ? Direction.DownRight : 0);

        switch (neighborsInRooms)
        {
            case Direction.None:
                return null;

            case Direction.UpLeft:
                return wallStyle.NECorner;

            case Direction.UpRight:
                return wallStyle.NWCorner;

            case Direction.DownLeft:
                return wallStyle.SECorner;

            case Direction.DownRight:
                return wallStyle.SWCorner;

            case Direction.Up | Direction.Down | Direction.Left | Direction.Right:
                if (wallStyle.Cross != null)
                    return wallStyle.Cross;
                throw new InvalidOperationException("WallStyle does not include a tile for cross junctions");

            case Direction.UpLeft | Direction.UpRight:
            case Direction.UpLeft | Direction.UpRight | Direction.DownLeft | Direction.Down | Direction.DownRight:
                return wallStyle.TJunctionDown;

            case Direction.DownLeft | Direction.DownRight:
            case Direction.DownLeft | Direction.DownRight | Direction.UpLeft | Direction.Up | Direction.UpRight:
                return wallStyle.TJunctionUp;

            case Direction.UpLeft | Direction.DownLeft:
                return wallStyle.TJunctionLeft;

            case Direction.UpRight | Direction.DownRight:
                return wallStyle.TJunctionRight;

            case Direction.UpLeft /*| Direction.Up */ | Direction.UpRight
             | Direction.Right
             | Direction.DownRight | Direction.Down | Direction.DownLeft
             | Direction.Left:
                return wallStyle.DownCap;

            case Direction.UpLeft | Direction.Up | Direction.UpRight
             | Direction.Right
             | Direction.DownRight /*| Direction.Down */ | Direction.DownLeft
             | Direction.Left:
                return wallStyle.UpCap;

            case Direction.UpLeft | Direction.Up | Direction.UpRight
             /*| Direction.Right*/
             | Direction.DownRight | Direction.Down | Direction.DownLeft
             | Direction.Left:
            case Direction.UpLeft | Direction.Up /*| Direction.UpRight*/
                /*| Direction.Right*/
         /*| Direction.DownRight*/ | Direction.Down | Direction.DownLeft
         | Direction.Left:
                return wallStyle.RightCap;

            case Direction.UpLeft | Direction.Up | Direction.UpRight
             | Direction.Right
             | Direction.DownRight | Direction.Down | Direction.DownLeft
             /*| Direction.Left*/:
                return wallStyle.LeftCap;

            default:
                if (upRoom || downRoom)
                {
                    if (leftRoom)
                        return wallStyle.RightCap;
                    if (rightRoom)
                        return wallStyle.LeftCap;
                    return wallStyle.Horizontal;
                }
                if (leftRoom || rightRoom)
                {
                    return wallStyle.Vertical;
                }
                return null;
        }
    }
}
