import React, { FunctionComponent } from "react";
import Pagination from "./Pagination";
import { Room } from "../types";
import ListCard from "./ListCard";

interface RoomListProps {
  rooms: Room[] | null | undefined;
  currPage: number;
  setCurrPage: React.Dispatch<React.SetStateAction<number>>;
  totalElements: number;
  pageSize: number;
}

const RoomList: FunctionComponent<RoomListProps> = ({
  rooms,
  currPage,
  setCurrPage,
  totalElements,
  pageSize,
}) => {
  return !rooms ? (
    <div>loading</div>
  ) : rooms.length < 1 ? (
    <div>no rooms</div>
  ) : (
    <div className="grid grid-rows-6 gap-4 h-full">
      {/* ListView */}
      <div className="row-span-5 grid grid-rows-4">
        {rooms?.map((room, i) => (
          <ListCard room={room} key={i} idx={i} />
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
