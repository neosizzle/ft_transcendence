import React, { FunctionComponent } from "react";
import Pagination from "./Pagination";
import { Room } from "../types";
import ListCard from "./ListCard";

interface RoomListProps {
  rooms: Room[] | null;
  currPage: number;
  setCurrPage: React.Dispatch<React.SetStateAction<number>>;
  totalElements: number;
  pageSize: number;
  setActiveRoom : React.Dispatch<React.SetStateAction<Room | null>>;
}

const RoomList: FunctionComponent<RoomListProps> = ({
  rooms,
  currPage,
  setCurrPage,
  totalElements,
  pageSize,
  setActiveRoom
}) => {
  return (
    <div className="grid grid-rows-6 gap-4 h-full">
      {/* ListView */}
      <div className="row-span-5 grid grid-rows-4">
        {rooms?.map((room, i) => (
          <ListCard room={room} key={i} setActiveRoom={setActiveRoom} />
        ))}
      </div>

      {/* Pagination */}
      <div className="row-span-1">
        <Pagination
          currPage={currPage}
          setCurrPage={setCurrPage}
          totalElements={totalElements}
          pageSize={pageSize}
        />
      </div>
    </div>
  );
};

export default RoomList;
