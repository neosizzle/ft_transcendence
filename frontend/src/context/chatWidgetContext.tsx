import React, { createContext, useContext, useState } from "react";
import { Room } from "../ChatWidget/types";

interface Props {
  children: React.ReactNode;
}

export interface LastMessage {
  roomId: number;
  message: string; // change to message dto soon
}

interface ChatWidgetCtx {
  currActiveRoom: Room | null;
  setCurrActiveRoom: React.Dispatch<React.SetStateAction<Room | null>>;
  rooms: Room[] | null;
  setRooms: React.Dispatch<React.SetStateAction<Room[] | null>>;
  notify: number[] | null;
  setNotify: React.Dispatch<React.SetStateAction<number[] | null>>;
  lastMessages: LastMessage[] | null;
  setLastMessages: React.Dispatch<React.SetStateAction<LastMessage[] | null>>;
}

const ChatWidgetContext = createContext<ChatWidgetCtx | null>(null);

export const ChatWidgetProvider = (props: Props) => {
  const [currActiveRoom, setCurrActiveRoom] = useState<Room | null>(null); // active viewing room
  const [rooms, setRooms] = useState<Room[] | null>(null); // all avail rooms
  const [notify, setNotify] = useState<number[] | null>(null); // notified rooms
  const [lastMessages, setLastMessages] = useState<LastMessage[] | null>(null); // last messages for all rooms

  return (
    <ChatWidgetContext.Provider
      value={{
        currActiveRoom,
        setCurrActiveRoom,
        rooms,
        setRooms,
        notify,
        setNotify,
        lastMessages,
        setLastMessages,
      }}
    >
      {props.children}
    </ChatWidgetContext.Provider>
  );
};

export const useChatWidget = () => {
  return useContext(ChatWidgetContext);
};
