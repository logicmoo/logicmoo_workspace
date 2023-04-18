export const nodeDataArray = [
    {
        key: 'user',
        text: 'Typical user flow',
        icon: 'user',
        color: 'pink'
    },
    {
        key: 'hl',
        text: 'Pâtisserie lesprit',
        isGroup: true,
        strokeDash: [1, 8]
    },
    {
        key: 'stp',
        text: 'Landing page',
        group: 'hl',
        icon: 'file',
        color: '#f4f0ea'
    },
    {
        key: 'ang',
        text: 'Angular app',
        group: 'hl',
        isGroup: true,
        color: '#fff4b8'
    },
    {
        key: 'quo',
        text: 'Quotes',
        group: 'ang',
        isGroup: true,
        color: 'rgba(0,0,0,.1)'
    },
    {
        key: 'quo-tool',
        text: 'Get started',
        group: 'quo',
        icon: 'file',
        color: '#f4f0ea'
    },
    {
        key: 'app',
        text: 'Questionnaire',
        group: 'ang',
        isGroup: true,
        color: 'rgba(0,0,0,.1)'
    },
    {
        key: 'quo-docker',
        group: 'quo',
        isGroup: true,
        color: '#189ec3',
        collapsible: true
    },
    {
        key: 'docker-quotes',
        text: 'quotes',
        group: 'quo-docker',
        icon: 'package',
        color: '#ffe157'
    },
    {
        key: 'ham',
        text: 'Accounts',
        isGroup: true,
        color: '#26a668'
    },
    {
        key: 'ham-create-acc',
        text: 'Create account',
        group: 'ham',
        icon: 'file',
        color: '#f4f0ea',
        pos: '980 -599'
    },
    {
        key: 'ham-docker',
        group: 'ham',
        isGroup: true,
        color: '#189ec3',
        collapsible: true
    },
    {
        key: 'docker-ham-app',
        text: 'accounts',
        group: 'ham-docker',
        icon: 'package',
        color: '#ffe157',
        pos: '1193 -558'
    },

    {
        key: 'docker-cookie-monster',
        text: 'cookies',
        group: 'ham-docker',
        icon: 'package',
        color: '#ffe157',
        pos: '1365 -558'
    },
    {
        key: 'db',
        text: 'Database',
        isGroup: true,
        color: '#26a668'
    },
    {
        key: 'db-dtc',
        text: 'pts-lesprit',
        group: 'db',
        icon: 'database',
        color: '#c4c4c4',
        pos: '213 1275'
    },
    {
        key: 'database-docker',
        group: 'db',
        isGroup: true,
        color: '#189ec3',
        collapsible: true
    },

    {
        key: 'docker-database',
        text: 'database',
        group: 'database-docker',
        icon: 'package',
        color: '#ffe157',
        pos: '428 1275'
    },
    {
        key: 'app-form',
        text: 'Questionnaire form',
        group: 'app',
        icon: 'file',
        color: '#f4f0ea'
    },
    {
        key: 'off',
        text: 'Issue',
        group: 'ang',
        isGroup: true,
        color: 'rgba(0,0,0,.1)'
    },
    {
        key: 'off-select-coverage',
        text: 'Select your plan',
        group: 'off',
        icon: 'file',
        color: '#f4f0ea'
    },
    {
        key: 'off-signature',
        text: 'Payment',
        group: 'off',
        isGroup: true,
        color: 'rgba(0,0,0,.1)'
    },
    {
        key: 'esign-docker',
        group: 'off-signature',
        isGroup: true,
        color: '#189ec3',
        collapsible: true
    },
    {
        key: 'off-select-coverage',
        text: 'payment',
        group: 'esign-docker',
        icon: 'package',
        color: '#ffe157'
    },
    {
        key: 'off-signature-esign',
        text: 'Payment',
        group: 'off-signature',
        icon: 'file',
        color: '#f4f0ea'
    },
    {
        key: 'iss',
        text: 'Account center',
        group: 'ang',
        isGroup: true,
        color: 'rgba(0,0,0,.1)'
    },
    {
        key: 'iss-account-center',
        text: 'Account center',
        group: 'iss',
        icon: 'file',
        color: '#f4f0ea'
    }
];

export const linkDataArray = [
    { from: 'user', to: 'stp', category: 'dashed' },
    { from: 'stp', to: 'quo-tool', category: 'dashed' },
    { from: 'quo-tool', to: 'ham-create-acc', category: 'dashed' },
    { from: 'ham-create-acc', to: 'app-form', category: 'dashed' },
    { from: 'app-form', to: 'off-select-coverage', category: 'dashed' },
    {
        from: 'off-select-coverage',
        to: 'off-signature-esign',
        category: 'dashed'
    },
    { from: 'off-signature-esign', to: 'iss-account-center', category: 'dashed' },

    {
        from: 'prd',
        to: 'ang',
        text: 'calculates premium values',
        bidirectional: true
    },
    {
        from: 'db-dtc',
        to: 'ang',
        text: 'stores app data',
        bidirectional: true
    },

    {
        from: 'pa',
        to: 'iss',
        text: 'administrates issued policies',
        bidirectional: true
    },

    {
        from: 'uw',
        to: 'dec',
        text: 'processes submitted applications',
        bidirectional: true
    }
];
