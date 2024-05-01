import { useRef, useState, useEffect } from 'react';
import { editor as monaco, KeyCode } from 'monaco-editor/esm/vs/editor/editor.api';
import styles from './Editor.module.css';

const analyze = () => alert("todo websocket")

// value: ['function x() {', '\tconsole.log("Hello world!");', '}'].join('\n'),
// language: 'typescript'
const Editor = () => {
	const [editor, setEditor] = useState<monaco.IStandaloneCodeEditor | null>(null);
	const editorRef = useRef(null);

	useEffect(() => {
		// Effect setup.
		if (editorRef) {
			setEditor((editor) => {
				if (editor) return editor;

				return monaco
					.create(editorRef.current!, {
						value: ['{', '\t"jjj": 12', '}'].join('\n'),
						language: 'json'
					})
					.addCommand(KeyCode.F9, analyze)
			});
		}

		return () => {
			// Effect cleanup.
			editor?.dispose()
		};

	}, [editorRef.current]);

	return (
		<div className={styles.Editor} ref={editorRef}></div>
	);
};

export default Editor;
