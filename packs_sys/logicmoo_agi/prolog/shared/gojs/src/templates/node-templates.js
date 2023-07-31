import go from 'gojs';
const jquery = go.GraphObject.make;

export const defaultNodeTemplate = jquery(
    go.Node,
    'Vertical',
    new go.Binding('position', 'pos', go.Point.parse),
    // .makeTwoWay(
    //   go.Point.stringify
    // ),
    {
        locationSpot: go.Spot.Center
    },
    jquery(
        go.Panel,
        'Spot',
        jquery(
            go.Shape,
            'RoundedRectangle',
            {
                parameter1: 16,
                fill: 'white',
                width: 96,
                height: 96,
                strokeWidth: 4,
                alignment: new go.Spot(0.5, 0.5)
            },
            new go.Binding('fill', 'color')
        ),
        jquery(
            go.Picture,
            { maxSize: new go.Size(64, 64) },
            new go.Binding('source', 'icon', function (icon) {
                return `/img/${icon}.svg`;
            })
        )
    ),
    jquery(
        go.TextBlock,
        {
            margin: new go.Margin(8, 0, 0, 0),
            width: 152,
            textAlign: 'center',
            font: '600 1.25rem CircularXX, sans-serif'
        },
        // new go.Binding("text", "pos", function(str) {
        //   return `${
        //     str.split(" ").map(el => {
        //       return Math.round(el);
        //     })[0]
        //   } ${
        //     str.split(" ").map(el => {
        //       return Math.round(el);
        //     })[1]
        //   }`;
        // }),
        new go.Binding('text', 'text')
    )
);
