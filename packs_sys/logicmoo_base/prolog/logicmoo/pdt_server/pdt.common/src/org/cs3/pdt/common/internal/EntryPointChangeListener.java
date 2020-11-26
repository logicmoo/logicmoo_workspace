package org.cs3.pdt.common.internal;

import java.io.IOException;

import org.cs3.pdt.common.PDTCommonPlugin;
import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

public class EntryPointChangeListener implements IResourceChangeListener {

	@Override
	public void resourceChanged(IResourceChangeEvent event) {
		if (event.getDelta() == null) {
			return;
		}
		int kind = event.getDelta().getKind();
		if ((kind == IResourceDelta.ADDED || kind == IResourceDelta.CHANGED)) {
			for (IResourceDelta deltaChild : event.getDelta().getAffectedChildren()) {
				if (!(deltaChild.getResource() instanceof IProject)) {
					continue;
				}
				final IProject project = (IProject) deltaChild.getResource();
				final PDTProperties pdtProperties = PDTProperties.getPDTProperties(project);
				boolean projectHasNoEntryPoints = pdtProperties.getEntryPoints().isEmpty();
				if (deltaChild.findMember(new Path(".pdtproperties")) != null) {
					pdtProperties.loadPropertyFile();
					PDTCommonPlugin.getDefault().notifyDecorators();
				};
				if (projectHasNoEntryPoints) {
					continue;
				}
				class BooleanContainer {boolean b = false;}
				final BooleanContainer changedEntryPoints = new BooleanContainer();
				try {
					deltaChild.accept(new IResourceDeltaVisitor() {
						@Override
						public boolean visit(IResourceDelta delta) throws CoreException {
							if (delta.getResource() instanceof IFile && delta.getKind() == IResourceDelta.REMOVED) {
								IFile file = (IFile) delta.getResource();
								if (PDTCommonUtil.isPrologFile(file) && pdtProperties.isEntryPoint(file)) {
									try {
										changedEntryPoints.b = true;
										pdtProperties.removeEntryPointFile(file);
										IPath newPath = delta.getMovedToPath();
										if (newPath != null) {
											IFile newFile = ResourcesPlugin.getWorkspace().getRoot().getFile(newPath);
											if (newFile.getProject().equals(project)) {
												pdtProperties.addEntryPointFile(newFile);
											} else {
												PDTProperties.getPDTProperties(newFile.getProject()).addEntryPointFile(newFile);
											}
										}
									} catch (IOException e) {
										Debug.report(e);
									}
								}
							}
							return true;
						}
					});
					if (changedEntryPoints.b && PDTCommonUtil.hasActivePrologProcess()) {
						PDTCommonUtil.updateEntryPointsInProcess(PDTCommonUtil.getActivePrologProcess());
					}
				} catch (CoreException | PrologProcessException e) {
					Debug.report(e);
				}
			}
		}
	}

}
