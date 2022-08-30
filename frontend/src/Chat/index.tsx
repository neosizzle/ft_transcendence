import React, { useState, useEffect } from "react";
import { ALREADY_FRIENDS, API_ROOT, NOT_FRIENDS } from "../constants";
import { useAuth, User } from "../context/authContext";
import { auth_net_get } from "../utils";
import { ERR, INCOMING_BAN, INCOMING_KICK, INCOMING_MSG } from "../constants";
import { Member, Message, Room } from "./classes";
import { useNavigate } from "react-router-dom";
import { useChat } from "../context/chatContext";
import RoomList from "./components/RoomList";
import ChatArea from "./components/ChatArea";
import MemberList from "./components/MemberList";

const roomEndpoint = `${API_ROOT}/rooms`;
const chatEndpoint = `${API_ROOT}/chat`;
const memberEndpoint = `${API_ROOT}/members`;
const blocksEndpoint = `${API_ROOT}/blocks`;
const friendsEndpoint = `${API_ROOT}/friends`;

function Chat() {
  const auth = useAuth();
  const navigate = useNavigate();
  const chat = useChat();
  const [memberUsers, setMemberUsers] = useState<User[] | null>(null); // user objects for members

  // DM STATES
  const [isUserBlockedByYou, setIsUserBlockedByYou] = useState<boolean>(false);
  const [userFriendShipState, setUserFriendShipState] =
    useState<number>(NOT_FRIENDS);
  const [otherDmUser, setOtherDmUser] = useState<User | null>(null);

  const refreshPage = () => {
    auth_net_get(`${roomEndpoint}?page=1&pageSize=50`).then((data) => {
      const myRooms = data.data;
      const roomsArr: Room[] = [];
      myRooms.forEach((random: Room) => {
        roomsArr.push(random);
      });
      chat?.setRooms(roomsArr);
    });

    //For setting activeRoomMessages
    auth_net_get(
      `${chatEndpoint}?page=1&pageSize=50&filterOn=roomId&filterBy=1&sortBy=Ascending&sortOn=createdAt`
    ).then((data) => {
      const chats = data.data;
      const msgsArr: Message[] = [];
      chats.forEach((chat: Message) => {
        msgsArr.push(chat);
      });
      chat?.setActiveRoomMessages(msgsArr);
    });
    console.log("Refreshing page");
    console.log("state ", chat?.activeRoom?.id);
    console.log("ref ", chat?.activeRoomRef.current?.id);
  };

  const Test = () => {
    console.log("Hi I do nothing currently, am test");
    console.log(chat?.activeRoom?.id);
  };

  useEffect(() => {
    auth_net_get(`${roomEndpoint}?page=1&pageSize=50`).then((data) => {
      const myRooms = data.data;
      const roomsArr: Room[] = [];
      myRooms.forEach((room: Room) => {
        roomsArr.push(room);
      });
      chat?.setRooms(roomsArr);
    });
    if (!auth?.chatSocket) return;
    auth.chatSocket.on(INCOMING_MSG, refreshPage);
    auth.chatSocket.on(ERR, Test);
    auth.chatSocket.on(INCOMING_KICK, Test);
    auth.chatSocket.on(INCOMING_BAN, Test);

    return () => {
      // remove socket listeners
      auth?.chatWidgetSocket?.off(INCOMING_MSG, refreshPage);
      auth?.chatWidgetSocket?.off(ERR, Test);
      auth?.chatWidgetSocket?.off(INCOMING_KICK, Test);
      auth?.chatWidgetSocket?.off(INCOMING_BAN, Test);
    };
  }, [auth]);

  useEffect(() => {
    //For first time setting activeRoomMessages
    if (chat?.rooms != null) {
      chat.setActiveRoom(chat.rooms[0]);
      auth_net_get(
        `${chatEndpoint}?page=1&pageSize=50&filterOn=roomId&filterBy=${chat.rooms[0].id}&sortBy=Ascending&sortOn=createdAt`
      ).then((data) => {
        const chats = data.data;
        const msgsArr: Message[] = [];
        chats.forEach((chat: Message) => {
          msgsArr.push(chat);
        });
        chat?.setActiveRoomMessages(msgsArr);
      });
    }
  }, [chat?.rooms]);

  useEffect(() => {
    //For setting activeRoomMessages
    if (chat?.activeRoom != null) {
      auth_net_get(
        `${chatEndpoint}?page=1&pageSize=50&filterOn=roomId&filterBy=${chat.activeRoom.id}&sortBy=Ascending&sortOn=createdAt`
      ).then((data) => {
        // token expired
        if (data.error && data.error == "Forbidden") return navigate("/logout");

        const chats = data.data;
        const msgsArr: Message[] = [];
        chats.forEach((chat: Message) => {
          msgsArr.push(chat);
        });
        chat?.setActiveRoomMessages(msgsArr);
      });
      //Gets the members for display on the right
      auth_net_get(
        `${memberEndpoint}?page=1&pageSize=50&filterOn=roomId&filterBy=${chat.activeRoom.id}`
      ).then((data) => {
        // token expired
        if (data.error && data.error == "Forbidden") return navigate("/logout");

        const member = data.data;
        const userArr: User[] = [];

        member.forEach((member: Member) => {
          userArr.push(member.user);
        });
        setMemberUsers(userArr);

        // set dm states
        if (chat?.activeRoom?.type !== "DM") return;

        const otherUser =
          userArr[0].id === auth?.user?.id ? userArr[1] : userArr[0];
        setOtherDmUser(otherUser);

        // set user is blocked state
        auth_net_get(
          `${blocksEndpoint}?page=1&pageSize=1&filterOn=intraName&filterBy=${otherUser.intraName}`
        ).then((data) => {
          if (data.error && data.error == "Forbidden")
            return navigate("/logout");
          if (!data.total_elements) setIsUserBlockedByYou(false);
          else setIsUserBlockedByYou(true);
        });

        // set user is friends state
        auth_net_get(
          `${friendsEndpoint}?page=1&pageSize=1&filterOn=intraName,reqStatus&filterBy=${otherUser.intraName},APPROVED`
        ).then((data) => {
          if (data.error && data.error == "Forbidden")
            return navigate("/logout");
          if (!data.total_elements) setUserFriendShipState(NOT_FRIENDS);
          else setUserFriendShipState(ALREADY_FRIENDS);
        });
      });
    }
  }, [chat?.activeRoom]);

  return (
    <>
      {/* Header */}
      <h1 className="text-5xl">Chat</h1>
      <div className="grid grid-cols-3">
        {/* Need to figure out how to get user avatar. If DM, get user.avatar as src string. Else if GC, use default? (Alternatively
						can use owner picture) */}

        {/* Room list */}
        <RoomList />

        {/* Chat area */}
        <ChatArea
          memberUsers={memberUsers}
          otherDmUser={otherDmUser}
          isUserBlockedByYou={isUserBlockedByYou}
          setIsUserBlockedByYou={setIsUserBlockedByYou}
          userFriendShipState={userFriendShipState}
          setUserFriendshipState={setUserFriendShipState}
        />

        {/* Members list */}
        <MemberList memberUsers={memberUsers}/>
      </div>
    </>
  );
}

export default Chat;
