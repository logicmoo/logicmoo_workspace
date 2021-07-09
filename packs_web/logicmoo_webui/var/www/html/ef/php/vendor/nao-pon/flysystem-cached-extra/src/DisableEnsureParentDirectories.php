<?php

namespace Hypweb\Flysystem\Cached\Extra;

trait disableEnsureParentDirectories
{
    /**
     * Disabled Ensure parent directories of an object.
     *
     * @param string $path object path
     */
    public function ensureParentDirectories($path) {}
}
