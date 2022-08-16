import React, { FunctionComponent } from "react";

interface LevelBarProps {
  percent?: number;
}

const LevelBar: FunctionComponent<LevelBarProps> = ({ percent }) => {
  return (
    <div
      className="
			flex
			justify-center
			pt-3
			"
    >
      <div className="w-5/6 h-6 bg-gray-200 rounded dark:bg-gray-700">
        <div
          className="h-6 bg-sky-400 rounded dark:bg-blue-500 flex justify-end"
          style={{ width: `${percent}%` }}
        ></div>
      </div>
    </div>
  );
};

export default LevelBar;
