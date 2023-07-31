<script>
    import { onMount } from 'svelte';
    import go from 'gojs';

    import { nodeDataArray, linkDataArray } from './data-arrays';
    import { defaultNodeTemplate } from './templates/node-templates';
    import { defaultGroupTemplate } from './templates/group-templates';
    import { defaultLinkTemplate, dashedLinkTemplate } from './templates/link-templates';

    const jquery = go.GraphObject.make;

    onMount(async () => {
        const appEl = document.getElementById('app');
        appEl.innerHTML = '';

        const containerEl = document.createElement('div');
        appEl.appendChild(containerEl);

        const diagramEl = document.createElement('div');
        diagramEl.style.width = '100vw';
        diagramEl.style.height = '100vh';
        diagramEl.style.backgroundColor = 'white';
        containerEl.appendChild(diagramEl);

        const diagram = new go.Diagram(diagramEl);
        diagram.model = new go.GraphLinksModel(nodeDataArray, linkDataArray);
        diagram.initialAutoScale = go.Diagram.Uniform;
        // diagram.layout = new go.ForceDirectedLayout();

        // Add Legend
        diagram.add(
            jquery(
                go.Part,
                'Table',
                {
                    position: new go.Point(213, -599),
                    selectable: false
                }, // end row 0
                jquery(
                    go.Panel,
                    'Horizontal',
                    { row: 1, alignment: go.Spot.Left },
                    jquery(go.Shape, 'RoundedRectangle', {
                        desiredSize: new go.Size(64, 64),
                        fill: '#26a668',
                        strokeWidth: 4,
                        margin: 16
                    }),
                    jquery(go.TextBlock, 'External unit', {
                        font: '600 2rem CircularXX, sans-serif'
                    })
                ), // end row 1
                jquery(
                    go.Panel,
                    'Horizontal',
                    { row: 2, alignment: go.Spot.Left },
                    jquery(go.Shape, 'RoundedRectangle', {
                        desiredSize: new go.Size(64, 64),
                        fill: '#189ec3',
                        strokeWidth: 4,
                        margin: 16
                    }),
                    jquery(go.TextBlock, 'Related containers', {
                        font: '600 2rem CircularXX, sans-serif'
                    })
                ) // end row 2
            )
        );

        function animate() {
            const animation = new go.Animation();
            diagram.links.each(link => {
                const dashedLink = link.findObject('dashedLink');
                if (dashedLink) {
                    animation.add(dashedLink, 'strokeDashOffset', 24, 0);
                }
            });

            animation.easing = go.Animation.EaseLinear;
            animation.runCount = Infinity;
            animation.start();
        }

        const nodeMap = new go.Map();
        const groupMap = new go.Map();
        const linkMap = new go.Map();

        nodeMap.add('', defaultNodeTemplate);
        groupMap.add('', defaultGroupTemplate);
        linkMap.add('', defaultLinkTemplate);
        linkMap.add('dashed', dashedLinkTemplate);

        diagram.nodeTemplateMap = nodeMap;
        diagram.groupTemplateMap = groupMap;
        diagram.linkTemplateMap = linkMap;

        animate();
    });
</script>

<style>
</style>

<div id="app"></div>
