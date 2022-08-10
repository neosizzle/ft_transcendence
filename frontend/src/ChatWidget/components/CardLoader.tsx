import React, { FunctionComponent } from "react";

const CardLoader: FunctionComponent = () => {
  return (
    <div className="border-b-2 grid grid-cols-6 animate-pulse ">
      {/* Pic */}
      <div className="col-span-1 bg-slate-400"></div>

      {/* Info */}
      <div className="col-span-4 px-2 bg-slate-400">
        <div className="text-md lg:text-lg"></div>
      </div>

      {/* Action */}
      <div className="col-span-1 bg-slate-400"></div>
    </div>
  );
};

export default CardLoader;
