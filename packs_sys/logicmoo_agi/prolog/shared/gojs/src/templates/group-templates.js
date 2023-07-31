import go from 'gojs';
const jquery = go.GraphObject.make;

export const defaultGroupTemplate = jquery(
    go.Group,
    'Vertical',
    { margin: 96 },
    jquery(
        go.Panel,
        'Auto',
        { defaultAlignment: go.Spot.TopLeft },
        jquery(
            go.Shape,
            'RoundedRectangle', // surrounds the Placeholder
            {
                parameter1: 16,
                fill: 'rgba(128,128,128,0.25)',
                strokeWidth: 4,
                strokeCap: 'round'
                // stroke: "#79797c",
            },
            new go.Binding('fill', 'color'),
            new go.Binding('stroke', 'stroke'),
            new go.Binding('strokeDashArray', 'strokeDash')
        ),
        jquery(
            go.Placeholder, // represents the area of all member parts,
            { padding: 32 }
        ), // with some extra padding around them
        jquery(
            go.Panel,
            'Horizontal', // the header
            jquery(
                'SubGraphExpanderButton',
                {
                    margin: new go.Margin(8),
                    scale: 2,
                    _buttonFillNormal: 'white',
                    _buttonStrokeNormal: 'black',
                    _buttonFillOver: 'rgba(128,128,128,0.25)',
                    _buttonStrokeOver: 'black',
                    'ButtonBorder.stroke': 'black',
                    'ButtonBorder.strokeWidth': 2,
                    visible: false
                },
                new go.Binding('visible', 'collapsible', function (t) {
                    return !!t;
                })
            )
        )
    ),
    jquery(
        go.TextBlock, // group title
        {
            font: '600 1.75rem CircularXX, sans-serif',
            margin: new go.Margin(8, 0, 0, 0)
        },
        new go.Binding('text', 'text')
    )
);
