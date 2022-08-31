import React, { FunctionComponent, useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { API_ROOT } from "../../constants";
import { useAuth, User } from "../../context/authContext";
import { useChat } from "../../context/chatContext";
import { auth_net_get } from "../../utils";
import { Admin, Member } from "../classes";
import MemberCard from "./MemberCard";

const adminEndpoint = `${API_ROOT}/admins`;
const blocksEndpoint = `${API_ROOT}/blocks`;
const friendsEndpoint = `${API_ROOT}/friends`;

interface MemberListProps {
  memberUsers: User[] | null;
}

const MemberList: FunctionComponent<MemberListProps> = ({ memberUsers }) => {
  // TODO, make these useref
  const [admins, setAdmins] = useState<Admin[]>([]);
  const chat = useChat();
  const auth = useAuth();
  const navigate = useNavigate();

  // initialize member block and friend values
  useEffect(() => {
    // evaluate blocks
    auth_net_get(
      `${blocksEndpoint}?page=1&pageSize=1`
    ).then(async (data) => {
      // token expired
      if (data.error && data.error == "Forbidden") return navigate("/logout");

      if (!data.total_elements) return;
      const blocks = await auth_net_get(
        `${blocksEndpoint}?page=1&pageSize=${data.total_elements}`
      );
      chat?.setActiveRoomBlocks(blocks.data);
    });

    // evaluate friends
    auth_net_get(`${friendsEndpoint}?page=1&pageSize=1`).then(async (data) => {
      // token expired
      if (data.error && data.error == "Forbidden") return navigate("/logout");

      if (!data.total_elements) return;
      const friends = await auth_net_get(
        `${friendsEndpoint}?page=1&pageSize=${data.total_elements}`
      );
      chat?.setActiveRoomFriends(friends.data);
    });
  }, [memberUsers]);

  // initialize Gc specific values
  useEffect(() => {
    // return if dm
    if (!chat || chat?.activeRoom?.type !== "GC") return;

    // evaluate all admins
    // evaluate total number of admins in room
    auth_net_get(
      `${adminEndpoint}?page=1&pageSize=1&filterOn=roomId&filterBy=${chat.activeRoom.id}`
    ).then(async (data) => {
      if (data.error && data.error == "Forbidden") return navigate("/logout");

      // evaluate all admins in room
      if (!data.total_elements) return;
      const adminsRes = await auth_net_get(
        `${adminEndpoint}?page=1&pageSize=${data.total_elements}&filterOn=roomId&filterBy=${chat?.activeRoom?.id}`
      );
      setAdmins(adminsRes.data);
    });
  }, [memberUsers]);

  return (
    <div>
      <div className="border-2 text-center text-5xl">Members</div>

      <div>
        {memberUsers?.map((user) => (
          <MemberCard
            userBlocked={
              (chat?.activeRoomBlocks?.findIndex(
                (block) => block.blockeeId === user.id
              ) as number) >= 0
            }
            friendship={chat?.activeRoomFriends.find(
              (friend) =>
                friend.friendId === user.id || friend.userId === user.id
            )}
            user={user}
            key={user.id}
            isAdmin={admins.findIndex((admin) => admin.userId === user.id) >= 0}
            isOwner={chat?.activeRoom?.ownerId === user.id}
            isSelf={user.id === auth?.user?.id}
          />
        ))}
      </div>

      {/* Time selection modal for mute / ban */}

      {/* Confirmation modal for owner transfer */}
    </div>
  );
};

export default MemberList;
