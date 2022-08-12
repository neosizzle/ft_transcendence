import React, { FunctionComponent, useEffect, useState } from "react";
import { API_ROOT } from "../constants";
import { useAuth } from "../context/authContext";
import { auth_net_get } from "../utils";
import { BaseWSResponse, memberDto, roomDto, SocketInterface } from "./classes";

/**
 * States required
 * 
 * currActiveRoom
 * rooms
 * notify
 * socketInterface
 * activeroommessages
 * activeroomlastmsg
 */

/**
 * Incoming new message
 *
 * 1. Check if message is current active room. If yes, append message to list of messages
 * 2. If no, check of roomid is in list. If yes, enable notification at roomId
 * 3. If no, append room to roomlist and append message
 * @param data data from ws server
 */
const handleNewMsg = (data: BaseWSResponse) => {
  console.log(data);
  alert(data.message);
};

/**
 * Incoming error event
 *
 * 1. Display error popup / dialog
 * @param data data from ws server
 */
const handleError = (data: BaseWSResponse) => {
  console.log(data);
  alert("ERROR " + data.message);
};

/**
 * Handle owner change
 *
 * 1. If owner is not curr user, remove user owner status
 * 2. Add owner indication on new owner
 */
const handleOwnerChange = (data: BaseWSResponse) => {
  console.log(data);
  alert(data.message);
};

/**
 * Handle Kick
 *
 * 1. If kicked user is self, leave room
 * 2. If not self, update member list
 */
const handleKick = (data: BaseWSResponse) => {
  console.log(data);
  alert(data.message);
};

/**
 * Handle Ban
 *
 * 1. If banned user is self, leave room
 * 2. If not self, update member list
 */
const handleBan = (data: BaseWSResponse) => {
  console.log(data);
  alert(data.message);
};

/**
 * Handle Promotion
 *
 * 1. If self is promoted, enable admin privelleges
 */
const handlePromotion = (data: BaseWSResponse) => {
  console.log(data);
  alert(data.message);
};

/**
 * Handle Demotion
 *
 * 1. If self is demoted, enable admin privelleges
 */
const handleDemotion = (data: BaseWSResponse) => {
  console.log(data);
  alert(data.message);
};

const ROOM_TO_JOIN = 146;
const member_endpoint = `${API_ROOT}/members`;

const WsRoot: FunctionComponent = () => {
  const socket = new SocketInterface(
    handleNewMsg,
    handleError,
    handleOwnerChange,
    handleKick,
    handleBan,
    handlePromotion,
    handleDemotion
  );
  const auth = useAuth();

  useEffect(() => {
    return () => {
      socket.destroy();
    };
  }, []);

  const handleCreate = () => {
    const dto: roomDto = {
      type: "GC",
      roomName: "fromfront",
      initialUsers: "",
    };
    socket?.socket?.emit("create", JSON.stringify(dto));
  };

  const handleJoin = () => {
    const dto: memberDto = { roomId: ROOM_TO_JOIN };
    socket?.socket?.emit("join", JSON.stringify(dto));
  };

  const handleLeave = async () => {
    const member = await auth_net_get(
      `${member_endpoint}?filterBy=${auth?.user?.id},${ROOM_TO_JOIN}&filterOn=userId,roomId&page=1&pageSize=1`
    );
    console.log(member);
    // const dto : roomDto = {type : "GC", roomName : "fromfront", initialUsers : "145"};
    socket?.socket?.emit(`leave`, member.data[0].id);
  };

  return (
    <div>
      <button
        className="px-3 py-1 border border-gray-300"
        onClick={() => socket?.socket?.disconnect()}
      >
        disconnect
      </button>

      <button
        className="px-3 py-1 border border-gray-300"
        onClick={() => handleCreate()}
      >
        create room gc
      </button>

      <button
        className="px-3 py-1 border border-gray-300"
        onClick={() => handleLeave()}
      >
        leave room
      </button>

      <button
        className="px-3 py-1 border border-gray-300"
        onClick={() => handleJoin()}
      >
        join room
      </button>
    </div>
  );
};

const WsTest: FunctionComponent = () => {
  return <WsRoot />;
};

export default WsTest;
