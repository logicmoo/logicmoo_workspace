<script>
  import Diagram from './Diagram';
</script>

<style>
    :global(html) {
        box-sizing: border-box;
    }

    :global(*, *:before, *:after) {
        box-sizing: inherit;
    }

    :global(body) {
        margin: 0;
    }

    @font-face {
        font-family: 'openSans';
        src: url('https://fonts.googleapis.com/css2?family=Open+Sans');
        font-weight: 400;
    }

    @font-face {
        font-family: 'openSans';
        src: url('https://fonts.googleapis.com/css2?family=Open+Sans');
        font-weight: 600;
    }
</style>

<main class="app">
  <Diagram />
</main>