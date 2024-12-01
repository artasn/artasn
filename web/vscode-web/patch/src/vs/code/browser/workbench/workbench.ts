import {
    create
} from 'vs/workbench/workbench.web.main';
import { URI, UriComponents } from 'vs/base/common/uri';
import { IWorkbenchConstructionOptions, IWorkspace, IWorkspaceProvider } from 'vs/workbench/browser/web.api';

declare const window: Window & {
    createWorkbench: (config: WorkbenchConfig) => void;
};

interface WorkbenchConfig {
    options: IWorkbenchConstructionOptions;
    folderUri?: UriComponents;
    workspaceUri?: UriComponents;
    domElementId?: string;
}

window.createWorkbench = function (config: WorkbenchConfig) {
    if (Array.isArray(config.options.additionalBuiltinExtensions)) {
        const additionalBuiltinExtensions =
            config.options.additionalBuiltinExtensions.map((path) => URI.parse(`${window.location.protocol}//${window.location.host}${path}`));
        config.options = { ...config.options, additionalBuiltinExtensions };
    }

    let workspace: IWorkspace | undefined;
    if (config.folderUri) {
      workspace = { folderUri: URI.revive(config.folderUri) };
    } else if (config.workspaceUri) {
      workspace = { workspaceUri: URI.revive(config.workspaceUri) };
    } else {
      workspace = undefined;
    }

    if (workspace) {
        const workspaceProvider: IWorkspaceProvider = {
            workspace,
            open: async (
                _workspace: IWorkspace,
                _options?: { reuse?: boolean; payload?: object }
            ) => true,
            trusted: true,
        };
        config.options = { ...config.options, workspaceProvider };
    }

    const domElement = !!config.domElementId
        && document.getElementById(config.domElementId)
        || document.body;

    create(domElement, config.options);
};
