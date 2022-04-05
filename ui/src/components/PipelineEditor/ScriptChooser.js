import './Editor.css'

import {React, useState, useEffect} from 'react';

const BonInABoxScriptService = require('bon_in_a_box_script_service');

export default () => {
  const api = new BonInABoxScriptService.DefaultApi();
  const [scriptFiles, setScriptFiles] = useState([]);

  // Applied only once when first loaded  
  useEffect(() => {
    // Load list of scripts into scriptFileOptions
    console.log("scriptListGet");
    api.scriptListGet((error, data, response) => {
      if (error) {
        console.error(error);
      } else {
        console.log(data);
        setScriptFiles(data);
      }
    });
    // Empty dependency array to get script list only once
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  const onDragStart = (event, nodeType, descriptionFile) => {
    event.dataTransfer.setData('application/reactflow', nodeType);
    event.dataTransfer.setData('descriptionFile', descriptionFile);
    event.dataTransfer.effectAllowed = 'move';
  };

  return (
    <aside className='scriptChooser'>
      <div className="dndnode input" onDragStart={(event) => onDragStart(event, 'input')} draggable>
        Constant value
      </div>
      <div className="dndnode output" onDragStart={(event) => onDragStart(event, 'output')} draggable>
        Pipeline output
      </div>
      <div className="description">Available scripts:</div>
      {scriptFiles.map((descriptionFile) => {
        // TODO: a hierarchy?
        console.log(descriptionFile)
        return <div key={descriptionFile} className="dndnode" onDragStart={(event) => onDragStart(event, 'default', descriptionFile)} draggable>
          <pre>
            {descriptionFile.replace('>', '\n')}
          </pre>
        </div>
      })}
    </aside>
  );
};