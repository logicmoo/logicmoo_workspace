import go from 'gojs';
const jquery = go.GraphObject.make;

export const defaultLinkTemplate = jquery(
    go.Link,
    {},
    jquery(go.Shape, { strokeWidth: 4 }), // this is the link shape (the line)
    jquery(go.Shape, {
        toArrow: 'OpenTriangle',
        fromArrow: 'Chevron',
        strokeWidth: 2,
        scale: 2
    }), // this is an arrowhead
    jquery(
        go.Shape,
        {
            strokeWidth: 2,
            scale: 2
        },
        new go.Binding('fromArrow', 'bidirectional', function (value) {
            if (value) {
                return 'BackwardOpenTriangle';
            }
        })
    ), // this is an arrowhead

    jquery(
        go.TextBlock, // this is a Link label
        {
            font: '400 1.25rem CircularXX, sans-serif',
            background: 'white'
        },
        new go.Binding('text', 'text')
    )
);

export const dashedLinkTemplate = jquery(
    go.Link,
    { curve: go.Link.Bezier },
    jquery(go.Shape, {
        name: 'dashedLink',
        stroke: 'black',
        strokeWidth: 4,
        strokeDashArray: [16, 8],
        strokeJoin: 'round'
    }),
    jquery(go.Shape, {
        toArrow: 'OpenTriangle',
        fill: null,
        stroke: 'black',
        strokeWidth: 2,
        scale: 2,
        strokeCap: 'round',
        strokeJoin: 'round'
    })
);
