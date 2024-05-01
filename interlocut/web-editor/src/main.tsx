import { StrictMode } from 'react';
import { createRoot } from 'react-dom/client';
import Editor from './components/Editor';
import './userWorker';

// @ts-ignore
//
// Safety:
//   id="root" is present at index.html.
//
const root = createRoot(document.getElementById('root'));

root.render(
	<StrictMode>
		<Editor />
	</StrictMode>,
);
